/*global angular*/
/*global showBaselineTraitDetailsModal */
/* global openManageLocations*/
(function() {
	'use strict';

	angular.OrderedHash = (function() {
		function OrderedHash() {
			this.m_keys = [];
			this.m_vals = {};
		}

		OrderedHash.prototype.addList = function(list, keyExtract) {
			if (list) {
				for (var i = 0; i < list.length; i++) {
					var _key = !isNaN(keyExtract(list[i])) ? Number(keyExtract(list[i])) : keyExtract(list[i]);

					this.m_keys.push(_key);
					this.m_vals[_key] = list[i];
				}
			}
		};

		OrderedHash.prototype.push = function(k, v) {
			var _key = !isNaN(k) ? Number(k) : k;
			if (!this.m_vals[k]) {
				this.m_keys.push(_key);
			}
			this.m_vals[_key] = v;
			return v;
		};

		OrderedHash.prototype.length = function() {
			return this.m_keys.length;
		};

		OrderedHash.prototype.keys = function() {
			return this.m_keys;
		};

		OrderedHash.prototype.val = function(k) {
			var _key = !isNaN(k) ? Number(k) : k;

			return this.m_vals[_key];
		};

		OrderedHash.prototype.vals = function() {
			return this.m_vals;
		};

		OrderedHash.prototype.remove = function(key) {
			var _key = !isNaN(key) ? Number(key) : key;

			this.m_keys.splice(this.m_keys.indexOf(_key), 1);
			delete this.m_vals[_key];

		};

		OrderedHash.prototype.removeAll = function() {
			while (this.m_keys.length > 0) {
				var _key = this.m_keys.pop();
				delete this.m_vals[_key];
			}
		};

		return OrderedHash;

	})();

	angular.module('fieldbook-utils', ['ui.select2', 'ui.select', 'datatables'])
		.constant('VARIABLE_SELECTION_MODAL_SELECTOR', '.vs-modal')
		.constant('VARIABLE_SELECTED_EVENT_TYPE', 'variable-select')
		.directive('displaySettings', ['TrialManagerDataService', '$filter', '_', 'studyStateService',
			function(TrialManagerDataService, $filter, _, studyStateService) {
			return {
				restrict: 'E',
				scope: {
					settings: '=',
					hideDelete: '=',
					predeleteFunction: '&'
				},
				templateUrl: '/Fieldbook/static/angular-templates/displaySettings.html',
				controller: function($scope, $element, $attrs) {


					$scope.$watch('settings', function(newValue, oldValue) {
						if (oldValue.m_keys.length !== newValue.m_keys.length) {
							$scope.options.selectAll = false;
						}
					}, true);

					$scope.variableType = $attrs.variableType;
					$scope.options = {
						selectAll: false
					};

					// when the selectAll checkbox is clicked, do this
					$scope.doSelectAll = function() {
						var filteredVariables = $filter('removeHiddenAndDeletablesVariableFilter')($scope.settings.keys(), $scope.settings.vals());

						_.each(filteredVariables, function(cvTermID) {
							$scope.settings.val(cvTermID).isChecked = $scope.options.selectAll;
						});

					};

					// when the delete button is clicked do this
					$scope.removeSettings = function() {

						if (typeof $scope.predeleteFunction() === 'undefined') {
							$scope.doDeleteSelectedSettings();
						} else {
							var checkedVariableTermIds = $scope.retrieveCheckedVariableTermIds($scope.settings);
							var promise = $scope.predeleteFunction()($attrs.variableType, checkedVariableTermIds);
							promise.then(function(doContinue) {
								if (doContinue) {
									$scope.doDeleteSelectedSettings();
								}
							});
						}
					};

					$scope.retrieveCheckedVariableTermIds = function(_settings) {
						var checkedCvtermIds = _.pairs(_settings.vals())
							.filter(function(val) {
								return _.last(val).isChecked;
							})
							.map(function(val) {
								return parseInt(_.first(val));
							});
						return checkedCvtermIds;
					};

					$scope.doDeleteSelectedSettings = function() {
						TrialManagerDataService.removeSettings($attrs.variableType, $scope.settings).then(function(data) {
							$scope.options.selectAll = false;
						});
					};

					$scope.size = function() {
						return Object.keys($scope.settings).length;
					};

					$scope.doSelect = function (isChecked) {
						if (!isChecked) {
							$scope.options.selectAll = false;
						}
					};
				}
			};
		}])
		.directive('datasetSettings', ['$filter', '_',
			function($filter, _) {
				return {
					restrict: 'E',
					scope: {
						settings: '=',
						hideDelete: '=',
						predeleteFunction: '&'
					},
					templateUrl: '/Fieldbook/static/angular-templates/displaySettings.html',
					controller: function($scope, $element, $attrs) {

						$scope.$watch('settings', function (newValue, oldValue) {
							if (oldValue.m_keys.length !== newValue.m_keys.length) {
								$scope.options.selectAll = false;
							}
						}, true);

						$scope.variableType = $attrs.variableType;
						$scope.options = {
							selectAll: false
						};

						$scope.doSelect = function (isChecked) {
							if (!isChecked) {
								$scope.options.selectAll = false;
							}
						};

						// when the selectAll checkbox is clicked, do this
						$scope.doSelectAll = function() {
							var filteredVariables = $filter('removeHiddenAndDeletablesVariableFilter')($scope.settings.keys(), $scope.settings.vals());

							_.each(filteredVariables, function(cvTermID) {
								$scope.settings.val(cvTermID).isChecked = $scope.options.selectAll;
							});

						};

						// when the delete button is clicked do this
						$scope.removeSettings = function() {
							if (typeof $scope.predeleteFunction() === 'undefined') {
								$scope.doDeleteSelectedSettings();
							} else {
								var checkedVariableTermIds = $scope.retrieveCheckedVariableTermIds($scope.settings);
								$scope.predeleteFunction()(checkedVariableTermIds, $scope.settings);
							}
						};

						$scope.retrieveCheckedVariableTermIds = function(_settings) {
							var checkedCvtermIds = _.pairs(_settings.vals())
								.filter(function(val) {
									return _.last(val).isChecked;
								})
								.map(function(val) {
									return parseInt(_.first(val));
								});
							return checkedCvtermIds;
						};

						$scope.doDeleteSelectedSettings = function() {

						};

						$scope.size = function() {
							return Object.keys($scope.settings).length;
						};
					}
				};
			}])
		.directive('validNumber', function() {

			return {
				require: '?ngModel',
				link: function(scope, element, attrs, ngModelCtrl) {
					if (!ngModelCtrl) {
						return;
					}

					ngModelCtrl.$parsers.push(function(val) {
						var clean = val.replace(/[^0-9]+/g, '');
						if (val !== clean) {
							ngModelCtrl.$setViewValue(clean);
							ngModelCtrl.$render();
						}
						return clean;
					});

					element.bind('keypress', function(event) {
						if (event.keyCode === 32) {
							event.preventDefault();
						}
					});
				}
			};
		})
		.directive('validDecimal', function() {

			return {
				require: '?ngModel',
				link: function(scope, element, attrs, ngModelCtrl) {
					if (!ngModelCtrl) {
						return;
					}

					ngModelCtrl.$parsers.push(function(val) {
						var clean = val.replace(/[^-?0-9.]+/g, '');
						if (val !== clean) {
							ngModelCtrl.$setViewValue(clean);
							ngModelCtrl.$render();
						}
						return clean;
					});

					element.bind('keypress', function(event) {
						if (event.keyCode === 32) {
							event.preventDefault();
						}
					});
				}
			};
		})
		.directive('selectStandardVariable', ['VARIABLE_SELECTION_MODAL_SELECTOR', 'VARIABLE_SELECTED_EVENT_TYPE', 'TrialSettingsManager', 'TrialManagerDataService',
			function(VARIABLE_SELECTION_MODAL_SELECTOR, VARIABLE_SELECTED_EVENT_TYPE, TrialSettingsManager, TrialManagerDataService) {
				return {
					restrict: 'A',
					scope: {
						modeldata: '=',
						callback: '&',
						selectedvariables: '=',
						selectVariableCallback: '=',
						onHideCallback: '='
					},

					link: function(scope, elem, attrs) {

						scope.processData = function(data) {
							scope.$apply(function() {
								var out = {};

								if (data.responseData) {
									data = data.responseData;
								}
								if (data) {

									// if retrieved data is an array of values
									if (data.length && data.length > 0) {
										$.each(data, function(key, value) {
											scope.modeldata.push(value.variable.cvTermId, TrialManagerDataService.transformViewSettingsVariable(value));
											out[value.variable.cvTermId] = value;
											scope.callback({ result: out });

										});
									} else {
										// if retrieved data is a single object
										scope.modeldata.push(data.variable.cvTermId, data);
									}

								}

								scope.$emit('variableAdded', out);
							});
						};

						elem.on('click', function() {

							var params = {
								variableType: attrs.variableType,
								retrieveSelectedVariableFunction: function () {
									if (!scope.selectedvariables) {
										var allSettings = TrialManagerDataService.getSettingsArray();
										var selected = {};

										angular.forEach(allSettings, function (tabSettings) {
											angular.forEach(tabSettings.vals(), function (value) {
												selected[value.variable.cvTermId] = value.variable.name;
											});
										});

										return selected;
									}
									return scope.selectedvariables;
								},
								callback: scope.selectVariableCallback,
								onHideCallback: scope.onHideCallback
							};

							$(VARIABLE_SELECTION_MODAL_SELECTOR).off(VARIABLE_SELECTED_EVENT_TYPE);
							$(VARIABLE_SELECTION_MODAL_SELECTOR).on(VARIABLE_SELECTED_EVENT_TYPE, scope.processData);

							TrialSettingsManager.openVariableSelectionDialog(params);
						});
					}
				};
			}])
		.directive('showSettingFormElement', ['_', function(_) {
			return {
				require: '?uiSelect2, ?ngModel',
				restrict: 'E',
				scope: {
					settings: '=',
					targetkey: '@targetkey',
					settingkey: '@',
					valuecontainer: '=',
					changefunction: '&',
					blockInput: '='
				},

				templateUrl: '/Fieldbook/static/angular-templates/showSettingFormElement.html',
				compile: function(tElement, tAttrs, transclude, uiSelect2) {
					if (uiSelect2) {
						uiSelect2.compile(tElement, tAttrs);
					}
				},
				controller: function($scope, LOCATION_ID, BREEDING_METHOD_ID, BREEDING_METHOD_CODE, $http) {
					if ($scope.settingkey === undefined) {
						$scope.settingkey = $scope.targetkey;
					}

					if (!$scope.changefunction) {
						$scope.changefunction = function() {
						};
					}

					$scope.variableDefinition = $scope.settings.val($scope.settingkey);
					$scope.widgetType = $scope.variableDefinition.variable.widgetType.$name ?
						$scope.variableDefinition.variable.widgetType.$name : $scope.variableDefinition.variable.widgetType;
					$scope.hasDropdownOptions = $scope.widgetType === 'DROPDOWN';

					$scope.isLocation = parseInt(LOCATION_ID, 10) === parseInt($scope.variableDefinition.variable.cvTermId, 10);

                    $scope.isBreedingMethod = parseInt(BREEDING_METHOD_ID, 10) === parseInt($scope.variableDefinition.variable.cvTermId, 10) ||
						parseInt(BREEDING_METHOD_CODE, 10) === parseInt($scope.variableDefinition.variable.cvTermId, 10);

					$scope.localData = {};
					var showAll = $scope.isBreedingMethod ? true : $scope.valuecontainer[$scope.targetkey];
					$scope.localData.useFavorites = !showAll;
					$scope.lookupLocation =  showAll ? 2 : 1;

					$scope.updateDropdownValuesFavorites = function() {
						if ($scope.localData.useFavorites) {
							if ($scope.lookupLocation == 1) {
								$scope.dropdownValues = $scope.variableDefinition.possibleValuesFavorite;
							} else {
								$scope.dropdownValues = $scope.variableDefinition.allFavoriteValues;
							}
						} else {
							if ($scope.lookupLocation == 1) {
								$scope.dropdownValues = $scope.variableDefinition.possibleValues;
							} else {
								$scope.dropdownValues = $scope.variableDefinition.allValues;
							}
						}
					};

					$scope.updateDropdownValuesBreedingLocation = function() { // Change state for breeding
						$scope.dropdownValues = ($scope.localData.useFavorites) ? $scope.variableDefinition.possibleValuesFavorite : $scope.variableDefinition.possibleValues;
						$scope.lookupLocation = 1;
					};

					$scope.updateDropdownValuesAllLocation = function() { // Change state for all locations radio
						$scope.dropdownValues = ($scope.localData.useFavorites) ? $scope.variableDefinition.allFavoriteValues : $scope.variableDefinition.allValues;
						$scope.lookupLocation = 2;
					};

					// if the value of the dropdown from existing data matches from the list of favorites, we set the checkbox as true
					var useFavorites = function(currentVal) {

						if (currentVal) {
							return false;
						} else if ($scope.variableDefinition.possibleValuesFavorite) {
							if($scope.isBreedingMethod){
								//If isBreedingMethod returns false
								false;
							}else{
								return $scope.variableDefinition.possibleValuesFavorite.length > 0;
							}
						}

						return $scope.localData.useFavorites;
					};

					if ($scope.hasDropdownOptions) {
                        var currentVal = $scope.valuecontainer[$scope.targetkey];

						// lets fix current val if its an object so that it only contains the id
						if($scope.isBreedingMethod){
							if (currentVal && currentVal.key) {
								currentVal = currentVal.key;
							}
						}else{
							if (typeof currentVal !== 'undefined' && currentVal !== null && typeof currentVal.id !== 'undefined' && currentVal.id) {
								currentVal = currentVal.id;
							}
						}


						$scope.localData.useFavorites = useFavorites(currentVal);

						$scope.updateDropdownValuesFavorites();
						$scope.lookUpValues = [];

						angular.forEach($scope.dropdownValues, function(value) {
							var idNumber;
							var curVal;
							if($scope.isBreedingMethod){
								if ($scope.valuecontainer[$scope.targetkey]) {
									idNumber = $scope.valuecontainer[$scope.targetkey];
								}
								$scope.lookUpValues[value.key] = value;
								curVal = value.key;
							}else{
								if (!isNaN($scope.valuecontainer[$scope.targetkey])) {
									idNumber = parseInt($scope.valuecontainer[$scope.targetkey]);
								}
								$scope.lookUpValues[value.id] = value;
								curVal = value.id;
							}

							$scope.lookUpValues[value.description] = value;
							if (value.description === $scope.valuecontainer[$scope.targetkey] ||
								curVal === idNumber) {
								$scope.valuecontainer[$scope.targetkey] = value;
							}
						});

                        $scope.$watch('valuecontainer[targetkey]', function() {
                        	if($scope.lookUpValues[$scope.valuecontainer[$scope.targetkey]]) {
                                $scope.valuecontainer[$scope.targetkey] = $scope.lookUpValues[$scope.valuecontainer[$scope.targetkey]];
							}
                        });

                    }

					// TODO: add code that can handle display of favorite methods, as well as update of possible values in case of click of manage methods
					if ($scope.isLocation) {
						$scope.clearArray = function(targetArray) {
							// current internet research suggests that this is the fastest way of clearing an array
							while (targetArray.length > 0) {
								targetArray.pop();
							}
						};

						$scope.updateLocationValues = function () {
							if (!$scope.variableDefinition.locationUpdated) {
								$http
									.get('/Fieldbook/locations/getLocations')
									.then(
										function (returnVal) {
											if (returnVal.data.success === '1') {
												$scope.variableDefinition.locationUpdated = true;
												// clear and copy of array is performed so as to preserve previous reference
												// and have changes applied to all components with a copy of the previous
												// reference
												$scope.clearArray($scope.variableDefinition.allValues);
												$scope.clearArray($scope.variableDefinition.possibleValues);
												$scope.clearArray($scope.variableDefinition.possibleValuesFavorite);
												$scope.clearArray($scope.variableDefinition.allFavoriteValues);

												$scope.variableDefinition.allValues.push.apply(
													$scope.variableDefinition.allValues, $scope
														.convertLocationsToPossibleValues(returnVal.data.allLocations));
												$scope.variableDefinition.possibleValues.push.apply(
													$scope.variableDefinition.possibleValues, $scope
														.convertLocationsToPossibleValues(returnVal.data.allBreedingLocations));
												$scope.variableDefinition.allFavoriteValues.push.apply(
													$scope.variableDefinition.allFavoriteValues, $scope
														.convertLocationsToPossibleValues(returnVal.data.favoriteLocations));
												$scope.variableDefinition.possibleValuesFavorite.push
													.apply(
														$scope.variableDefinition.possibleValuesFavorite,
														$scope
															.convertLocationsToPossibleValues(returnVal.data.allBreedingFavoritesLocations));
												$scope.updateDropdownValuesFavorites();
											}
										});
							}
						};

						$scope.convertLocationsToPossibleValues = function(locations) {
							var possibleValues = [];

							$.each(locations, function(key, value) {
								var locNameDisplay = value.lname;
								if (value.labbr != null && value.labbr != '') {
									locNameDisplay  += ' - (' + value.labbr + ')';
								}

								possibleValues.push({
									id: value.locid,
									name: locNameDisplay,
									description: value.lname
								});
							});

							return possibleValues;
						};

						$scope.initiateManageLocationModal = function() {
							$scope.variableDefinition.locationUpdated = false;
							openManageLocations();
						};

						$(document).off('location-update');
						$(document).on('location-update', $scope.updateLocationValues);
					}
				}
			};
		}])

		.directive('jqDatepicker', function() {
			return function(scope, element) {
				$(element).placeholder();
			};
		})


		.directive('inputType', function() {
			return {
				require: 'ngModel',
				link: function(scope, elem, attrs, ctrl) {
					// Custom number validation logic.
					if (attrs.inputType === 'number') {
						elem.attr('type', 'text');

						return ctrl.$parsers.push(function(value) {
							var valid = value === null || isFinite(value);

							ctrl.$setValidity('number', valid);

							return valid && value !== null ? Number(value) : undefined;
						});
					}

					// Fallback to setting the default `type` attribute.
					return elem.attr('type', attrs.inputType);
				}
			};
		})
		.directive('truncateAndTooltip', ['$compile', function($compile) {
			return {
				restrict: 'A',
				link: function(scope, element, attrs) {
					var length = 30;
					scope.$watch(attrs.truncateAndTooltip, function(newValue) {
						if (newValue && newValue.length > length) {
							element.attr('tooltip', newValue);
							element.attr('tooltip-placement', 'right');
							element.attr('tooltip-append-to-body', true);
							element.html(newValue.substring(0, length) + '...');

						} else {
							element.html(newValue);
						}

						// remove truncateAndTooltip attr so no infinite loop
						element.removeAttr('truncate-and-tooltip');

						$compile(element)(scope);
					});
				}
			};
		}])
		.directive('minVal', function() {
			return {
				require: 'ngModel',
				link: function(scope, elem, attrs, ctrl) {
					return ctrl.$parsers.push(function(value) {
						var valid = value === null || Number(value) >= Number(attrs.minVal);

						ctrl.$setValidity('min', valid);

						return valid ? value : undefined;
					});
				}
			};
		})

		.directive('maxVal', function() {
			return {
				require: 'ngModel',
				link: function(scope, elem, attrs, ctrl) {
					return ctrl.$parsers.push(function(value) {
						var valid = value === null || Number(value) <= Number(attrs.maxVal);

						ctrl.$setValidity('max', valid);

						return valid ? value : undefined;
					});
				}
			};
		})

		// filters
		.filter('range', function() {
			return function(input, total) {
				total = parseInt(total);
				for (var i = 0; i < total; i++) {
					input.push(i);
				}

				return input;
			};
		})

		.filter('removeHiddenVariableFilter', function() {
			return function(settingKeys, settingVals) {
				var keys = [];

				angular.forEach(settingKeys, function(val) {
					if (!settingVals[val].hidden) {
						keys.push(val);
					}
				});

				return keys;
			};
		})

		.filter('removeHiddenAndDeletablesVariableFilter', function() {
			return function(settingKeys, settingVals) {
				var keys = [];

				angular.forEach(settingKeys, function(val) {
					if (!settingVals[val].hidden && settingVals[val].deletable) {
						keys.push(val);
					}
				});

				return keys;
			};
		}).directive('instancesTable', ['DTOptionsBuilder', 'DTColumnBuilder', function (DTOptionsBuilder, DTColumnBuilder) {

			return {
				restrict: 'E',
				require: '?ngModel',
				scope: {
					instances: '=',
					selectedInstances: '=',
					isEmptySelection: '=',
					instanceIdProperty: '@'
				},
				templateUrl: '/Fieldbook/static/angular-templates/instancesTable.html',
				controller: function ($scope) {

					var ctrl = this;

					$scope.$watch('instances', function (newValue, oldValue, scope) {
							// Select All Checkbox by default.
						if (newValue.length !== 0) {
							$scope.toggleSelect(true);
							ctrl.isSelectAll = true;
						}
					});

					ctrl.isSelectAll = true;
					$scope.dtOptions = DTOptionsBuilder.newOptions().withDOM('<\'row\'<\'col-sm-6\'l><\'col-sm-6\'f>>' +
						'<\'row\'<\'col-sm-12\'tr>>' +
						'<\'row\'<\'col-sm-5\'i><\'col-sm-7\'>>' +
						'<\'row\'<\'col-sm-12\'p>>');

					$scope.toggleSelect = function (checked) {
						$.each($scope.instances, function (key, value) {
							$scope.selectedInstances[value[$scope.instanceIdProperty]] = checked;
						});
						$scope.selectionChanged();
					};

					$scope.select = function (itemId) {
						if (!$scope.selectedInstances[itemId]) {
							ctrl.isSelectAll = false;
						}
						$scope.selectionChanged();
					};

					$scope.selectionChanged = function() {
						// Returns true if all instances are not selected
						$scope.isEmptySelection = Object.values($scope.selectedInstances).every(function (value) {
							return value === false;
						});
					};

				},
				controllerAs: 'ctrl'
			};
		}]).directive('generateDesignInstancesTable', ['DTOptionsBuilder', 'DTColumnBuilder', function (DTOptionsBuilder, DTColumnBuilder) {

		return {
			restrict: 'E',
			require: '?ngModel',
			scope: {
				instances: '=',
				selectedInstances: '=',
				isEmptySelection: '=',
				canBeDeleted: '@',
				hasExperimentalDesign: '@',
				instanceNumber: '@',
			},
			templateUrl: '/Fieldbook/static/angular-templates/generateDesign/generateDesignInstancesTable.html',
			controller: function ($scope) {

				var ctrl = this;
				ctrl.isSelectAll = true;
				$scope.dtOptions = DTOptionsBuilder.newOptions().withDOM('<\'row\'<\'col-sm-6\'l><\'col-sm-6\'f>>' +
					'<\'row\'<\'col-sm-12\'tr>>' +
					'<\'row\'<\'col-sm-5\'i><\'col-sm-7\'>>' +
					'<\'row\'<\'col-sm-12\'p>>');

				$scope.$watch('instances', function (newValue, oldValue, scope) {
					if (newValue.length !== 0) {
						$.each($scope.instances, function (key, value) {
							$scope.selectedInstances[value[$scope.instanceNumber]] = !value[$scope.hasExperimentalDesign];
						});
						$scope.selectionChanged();
					}
				});

				$scope.toggleSelect = function (checked) {
					$.each($scope.instances, function (key, value) {
						if(value.canBeDeleted){
							$scope.selectedInstances[value[$scope.instanceNumber]] = checked;
						}
					});
					$scope.selectionChanged();
				};

				$scope.select = function (itemId) {
					if (!$scope.selectedInstances[itemId]) {
						ctrl.isSelectAll = false;
					}
					$scope.selectionChanged();
				};

				$scope.selectionChanged = function() {
					// Returns true if all instances are not selected
					$scope.isEmptySelection = Object.values($scope.selectedInstances).every(function (value) {
						return value === false;
					});

					//return true if canBeDeleted false
					ctrl.isSelectAll = Object.values($scope.selectedInstances).every(function (value, index) {
						return value === true || ($scope.instances[index].canBeDeleted === false);
					});
				};

			},
			controllerAs: 'ctrl'
		};
	}]).factory('formUtilities', function() {

			var formUtilities = {

				formGroupClassGenerator: function ($scope, formName) {
					return function (fieldName) {
						var className = 'form-group';

						// If the field hasn't been initialised yet, don't do anything!

						if ($scope[formName] && $scope[formName][fieldName]) {

							// Don't mark as invalid until we are relatively sure the user is finished doing things
							if ($scope[formName].$submitted || $scope[formName][fieldName].$touched) {

								// Only mark as invalid if the field is.. well, invalid
								if ($scope[formName][fieldName].$invalid) {
									className += ' has-error';
								}
							}
						}
						return className;
					};
				}
			}
			return formUtilities;
		})
		.filter('capitalize', function () {
			return function (inputString) {
				if (inputString !== undefined) {
					inputString = inputString.toLowerCase();
					if (inputString.indexOf(' ') !== -1) {
						return splitAndCapitalizeString(inputString, ' ');
					}
					else {
						return capitalizeString(inputString);
					}
				} else {
					return inputString;
				}

				function capitalizeString(inputString) {
					if (inputString.indexOf('-') !== -1) {
						return splitAndCapitalizeString(inputString, '-');
					}
					return inputString.substring(0, 1).toUpperCase() + inputString.substring(1);
				}

				function splitAndCapitalizeString(inputString, splitBy) {
					var inputPieces, i;
					inputString = inputString.toLowerCase();
					inputPieces = inputString.split(splitBy);

					for (i = 0; i < inputPieces.length; i++) {
						inputPieces[i] = capitalizeString(inputPieces[i]);
					}
					return inputPieces.toString().replace(/,/g, splitBy);

				}

			};
		})
		.factory('serviceUtilities', ['$q', function ($q) {
			return {
				restSuccessHandler: function (response) {
					return response.data;
				},

				restFailureHandler: function (response) {
					return $q.reject({
						status: response.status,
						data: response.data,
						errors: response.data && response.data.errors
					});
				}
			};
		}]).service('fileDownloadHelper', [function () {

			var fileDownloadHelper = {};

			fileDownloadHelper.save = function (blob, fileName) {

				var url = window.URL.createObjectURL(blob);

				// For IE 10 or later
				if (window.navigator && window.navigator.msSaveOrOpenBlob) {
					window.navigator.msSaveOrOpenBlob(url, fileName);
				} else { // For Chrome/Safari/Firefox and other browsers with HTML5 support
					var link = document.createElement('a');
					link.href = url;
					link.download = fileName;
					document.body.appendChild(link);
					link.click();
					document.body.removeChild(link);
				}
			};

			fileDownloadHelper.getFileNameFromResponseContentDisposition = function (response) {

				var contentDisposition = response.headers('content-disposition') || '';
				var matches = /filename=([^;]+)/ig.exec(contentDisposition);
				var fileName = (matches[1] || 'untitled').trim();
				return fileName;
			};

			return fileDownloadHelper;
		}]).service('helpLinkService',['$http',function ($http) {
			return {
				helpLink: function (value) {
					var config = {responseType: 'text', observe: 'response'};
					return $http({
						method: 'GET',
						url: '/ibpworkbench/controller/help/getUrl/' + value,
						responseType: 'text',
						transformResponse: undefined
					}).then(function (response) {
						return response.data;
					});
				}
			};
		}]);
	}
)();

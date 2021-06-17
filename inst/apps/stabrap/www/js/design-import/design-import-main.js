/**
 * Angular JS codes for the OWN design function, contains services, directives, and controller mainly used in the mapping page/modal
 * Created by cyrus on 05/12/15.
 */
/* global $, _, angular, bootbox, createErrorNotification, showAlertMessage, ImportDesign */
(function() {
	'use strict';

	var app = angular.module('designImportApp', ['ui.bootstrap', 'ngLodash', 'ngResource', 'ui.sortable']);

	app.controller('designImportCtrl', ['$scope', 'DesignMappingService', 'DesignOntologyService', 'ImportDesign', '$uibModal', 'Messages', function($scope, DesignMappingService, DesignOntologyService, ImportDesign, $uibModal, Messages) {
		// we can retrieve this from a service
		$scope.Messages = Messages;
		$scope.data = DesignMappingService.data;
		$scope.advancedOptions = {
			showAdvancedOptions: false,
			maintainHeaderNaming: false
		};

		$scope.toggleAdvancedOptions = function () {
			$scope.advancedOptions.showAdvancedOptions = !$scope.advancedOptions.showAdvancedOptions;
		};

		$scope.cancelMapping = function() {
			$scope.resetAdvancedOptions();
			ImportDesign.cancelDesignImport();
		};

		$scope.resetAdvancedOptions = function () {
			$scope.advancedOptions.showAdvancedOptions = false;
			$scope.advancedOptions.maintainHeaderNaming = false;
		};


		$scope.validateAndSend = function() {
			DesignMappingService.showConfirmIfHasUnmapped().then(function() {
				return DesignMappingService.validateMapping($scope.advancedOptions.maintainHeaderNaming);
			}, function() {
				return {cancelMapping: true};
			}).then(function(result) {
				$scope.resetAdvancedOptions();
				if (result.cancelDesignImport) {
					ImportDesign.cancelDesignImport();
					return;
				}

				if (!result.success) {
					createErrorNotification(Messages.DESIGN_MAPPING_ERROR_HEADER, result.error);
					return;
				}

				if (result.warning) {
					/** @namespace result.warning */
					showAlertMessage('', result.warning);
				}

				ImportDesign.showReviewPopup();
			}, function(failResult) {
				if (failResult.cancelMapping) {
					return;
				}

				var msg = Messages.DESIGN_IMPORT_MISSING_MAPPING_TEXT;
				createErrorNotification(Messages.DESIGN_MAPPING_ERROR_HEADER, msg);
			});

		};



		$scope.launchOntologyBrowser = function() {
			var $designMapModal = $('#designMapModal');
			$designMapModal.one('hidden.bs.modal', function() {
				setTimeout(function() {
					$scope.$apply(function() {
						var title = 'Ontology Browser';
						var url = '/ibpworkbench/controller/ontology';

						$uibModal.open({
							windowClass: 'modal-very-huge',
							controller: 'OntologyBrowserController',
							templateUrl: '/Fieldbook/static/angular-templates/ontologyBrowserPopup.html',
							resolve: {
								title: function() {
									return title;
								},

								url: function() {
									return url;
								}
							}
						}).result.finally(function() {
							// do something after this modal closes
							DesignOntologyService.clearData();

							setTimeout(function() {
								$designMapModal.modal('show');
							}, 200);

						});

					});
				}, 200);

			}).modal('hide');

		};

		$scope.designType = '';
		$scope.onDesignTypeSelect = function() {

			if ($scope.designType === '3') {
				// warning popup here
				showAlertMessage('', Messages.OWN_DESIGN_SELECT_WARNING);
			}

		};

	}]);

	app.controller('OntologyBrowserController', ['$scope', '$uibModalInstance', 'title', 'url',
		function($scope, $uibModalInstance, title, url) {
			$scope.title = title;
			$scope.url = url;
			$scope.close = function() {
				$uibModalInstance.dismiss('Cancelled');
			};

		}
	]);

	app.directive('mappingGroup', ['Messages', function(Messages) {
		return {
			restrict: 'E',
			scope: {
				name: '@',
				mappingData: '=data'
			},
			templateUrl: '/Fieldbook/static/angular-templates/designImport/mappingGroup.html',
			controller: ['$scope', '$attrs', function($scope, $attrs) {
				// data structure
				$scope.Messages = Messages;
				$scope.variableType = $attrs.variableType;

				$scope.sortableOptions = {
					connectWith: '.list-group',
					update: function(e, ui) {
						if (!ui.item.sortable.received) {
							var originNgModel = ui.item.sortable.sourceModel;
							var itemModel = originNgModel[ui.item.sortable.index];
							var dropNgModel = ui.item.sortable.droptargetModel;

							var exists = !!dropNgModel.filter(function(item) {
								return item.name === itemModel.name;
							}).length;

							// note ui.item.sortable.cancel() will interrupt the dragging

							if (!exists) {
								delete itemModel.variable;
								delete itemModel.required;
							}

						}
					}
				};

				$scope.computeButtonLabel = function(header) {
					if (header.variable) {
						return 'Re-map';
					} else {
						return 'Apply Mapping';
					}
				};

			}]

		};
	}]);

	app.directive('designMapVariableSelection', ['VARIABLE_SELECTION_MODAL_SELECTOR', 'VARIABLE_SELECTED_EVENT_TYPE', 'DesignOntologyService', 'Messages',
		function(VARIABLE_SELECTION_MODAL_SELECTOR, VARIABLE_SELECTED_EVENT_TYPE, DesignOntologyService, Messages) {
			return {
				restrict: 'A',
				scope: {
					modeldata: '=',
					callback: '&',
					mappedheader: '=mappedData'
				},

				link: function(scope, elem, attrs) {
					scope.processData = function(data) {
						scope.$apply(function() {
							if (data.responseData) {
								data = data.responseData;
							}
							if (data) {
								// if retrieved data is an array of values
								if (data.length && data.length > 0) {
									$.each(data, function(key, value) {
										scope.modeldata.id = value.variable.cvTermId;
										scope.modeldata.variable = value.variable;
									});
								}
							}

							$(VARIABLE_SELECTION_MODAL_SELECTOR).modal('hide');
						});
					};

					elem.on('click', function() {
						// temporarily close the current modal
						var $designMapModal = $('#designMapModal');

						var params = {
							variableType: attrs.group,
							retrieveSelectedVariableFunction: function() {
								return {};
							},
							callback: scope.processData,
							onHideCallback: function() {
								setTimeout(function() {
									$designMapModal.modal('show');
								}, 200);
							},
							apiUrl: '/Fieldbook/manageSettings/settings/role/' + attrs.group,
							options: {
								variableSelectBtnName: Messages.SELECT_TEXT,
								variableSelectBtnIco: 'glyphicon-chevron-right',
								noAlias: true
							},
							retrieveMappedTraits: function () {
								var mappedHeader = [];
								var mapped = scope.mappedheader;
								var object = {};
								$.each(mapped, function(index, header) {
									if(header.variable){
										mappedHeader[header.variable.id] = header.variable.alias || header.variable.name;
									}
								});
								return mappedHeader;
							}
						};

						$(VARIABLE_SELECTION_MODAL_SELECTOR).off(VARIABLE_SELECTED_EVENT_TYPE);
						$(VARIABLE_SELECTION_MODAL_SELECTOR).on(VARIABLE_SELECTED_EVENT_TYPE, scope.processData);

						$designMapModal.one('hidden.bs.modal', function() {
							setTimeout(function() {
								DesignOntologyService.openVariableSelectionDialog(params);
							}, 200);
						}).modal('hide');

					});
				}
			};
		}
	]);

	app.service('DesignMappingService', ['$http', '$q', '_', 'ImportDesign', 'Messages', function($http, $q, _, ImportDesign, Messages) {

		function validateMapping(maintainHeaderNaming) {

			var postData = angular.copy(service.data);
			var allMapped = true;
			var deferred = $q.defer();

			delete postData.unmappedHeaders;

			// lets grab all variables that are in groups but does not have mapped variables
			_.forEach(postData, function(value) {
				var results = _.filter(value, function(item) {
					return !_.has(item, 'variable');
				});

				if (results.length > 0) {
					allMapped = false;

					deferred.reject(results[0].name);

					return false;
				}
			});

			if (!allMapped) {
				return deferred.promise;
			}

			// transform postData into simpler list of standard variable ids
			// output should be in the format
			// result : { mappedDesignFactors : [ { name : header_name, id : std_var_id } ] }

			_.forIn(postData, function(value) {
				for (var i = 0; i < value.length; i++) {

					if (_.has(value[i], 'variable')) {
						if (_.has(value[i].variable, 'id') && value[i].variable.id) {
							value[i].id = value[i].variable.id;
						} else if (_.has(value[i].variable, 'cvTermId') && value[i].variable.cvTermId) {
							value[i].id = value[i].variable.cvTermId;
						} else {
							value[i].id = 0;
						}
						if(!maintainHeaderNaming) value[i].name = value[i].variable.alias || value[i].variable.name;
						value[i] = _.pick(value[i], ['id', 'name', 'columnIndex']);
					}

				}
			});

			var envCnt = ImportDesign.studyManagerCurrentData().instanceInfo.instances.length;

			return $http.post('/Fieldbook/DesignImport/validateAndSaveNewMapping/' + envCnt, postData).then(function(result) {
				var deferred = $q.defer();
				// note that angular $q promises is different from jquery's implem therefore we need to convert it to angular's defer()
				ImportDesign.hideDesignMapPopup().then(function() {
					deferred.resolve(result);
				});
				return deferred.promise;
			}).then(function(result) {
				return warnDesignOverwritePopup(result.data);
			});
		}

		function showConfirmIfHasUnmapped() {
			var hasUnmappedVariables = service.data.unmappedHeaders.length > 0;
			var deferred = $q.defer();

			if (!hasUnmappedVariables) {
				deferred.resolve(true);
			} else {
				bootbox.dialog({
					title: Messages.DESIGN_IMPORT_UNMAPPED_PROMPT_TITLE,
					message: Messages.DESIGN_IMPORT_UNMAPPED_PROMPT_TEXT,
					closeButton: false,
					onEscape: false,
					buttons: {
						yes: {
							label: Messages.YES,
							className: 'btn-primary',
							callback: function() {
								deferred.resolve(true);
							}
						},
						no: {
							label: Messages.NO,
							className: 'btn-default',
							callback: function() {
								deferred.reject(false);
							}
						}
					}
				});
			}

			return deferred.promise;
		}

		function warnDesignOverwritePopup(result) {
			var deferred = $q.defer();

			if (result.hasChecksSelected || result.hasExistingDesign)  {
				var dialogMessage = result.hasChecksSelected ? Messages.DESIGN_IMPORT_HAS_CHECKS_SELECTED_ALERT_MESSAGE : Messages.DESIGN_IMPORT_CONFLICT_ALERT_MESSAGE;

				bootbox.dialog({
					title: Messages.DESIGN_IMPORT_CONFLICT_ALERT_HEADER,
					message: dialogMessage,
					closeButton: false,
					onEscape: false,
					buttons: {
						yes: {
							label: Messages.YES,
							className: 'btn-primary',
							callback: function() {
								deferred.resolve(result);
							}
						},
						no: {
							label: Messages.NO,
							className: 'btn-default',
							callback: function() {
								result.cancelDesignImport = true;
								deferred.resolve(result);
							}
						}
					}
				});
			} else {
				deferred.resolve(result);
			}

			return deferred.promise;
		}

		var service = {
			data: {
				unmappedHeaders: [],
				mappedEnvironmentalFactors: [],
				mappedDesignFactors: [],
				mappedGermplasmFactors: [],
				mappedTraits: []
			},
			validateMapping: validateMapping,
			showConfirmIfHasUnmapped: showConfirmIfHasUnmapped
		};

		return service;

	}]);

	app.service('DesignOntologyService', ['VARIABLE_SELECTION_LABELS', function(VARIABLE_SELECTION_LABELS) {
		var TrialSettingsManager = window.TrialSettingsManager;
		var settingsManager = new TrialSettingsManager(VARIABLE_SELECTION_LABELS);

		return {
			openVariableSelectionDialog: function(params) {
				settingsManager._openVariableSelectionDialog(params);
			},

			// @param = this map contains variables of the pair that will be filtered
			addDynamicFilterObj: function(_map, group) {
				settingsManager._addDynamicFilter(_map, group);
			},
			clearData: function() {
				settingsManager._clearCache();
			}

		};
	}]);

	app.service('ImportDesign', function() {
		return ImportDesign;
	});

})();

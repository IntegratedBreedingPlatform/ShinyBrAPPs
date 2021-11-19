/**
 * Angular JS codes for the OWN design function, contains services, directives, and controller mainly used in the mapping page/modal
 * Created by cyrus on 05/12/15.
 */
/* global $, _, angular, bootbox, createErrorNotification, showAlertMessage */
(function () {
	'use strict';

	var app = angular.module('importObservationsApp', ['ui.bootstrap', 'ngLodash', 'ngResource', 'ui.sortable']);

	// TODO Refactor: Extract common functionality with designImportCtrl
	app.controller('importObservationsCtrl', ['$scope', 'ImportMappingService', 'DesignOntologyService', '$uibModal', 'Messages', 'datasetService', '$rootScope',
		function ($scope, ImportMappingService, DesignOntologyService, $uibModal, Messages, datasetService, $rootScope) {
			// we can retrieve this from a service
			$scope.Messages = Messages;
			$scope.data = ImportMappingService.data;
			$scope.datasetId = ImportMappingService.datasetId;

			$scope.advancedOptions = {
				showAdvancedOptions: false,
				maintainHeaderNaming: false,
			};

			$scope.toggleAdvancedOptions = function () {
				$scope.advancedOptions.showAdvancedOptions = !$scope.advancedOptions.showAdvancedOptions;
			};

			$scope.cancelMapping = function () {
				//Should continue with the import?
				$scope.resetAdvancedOptions();
			};

			$scope.resetAdvancedOptions = function () {
				$scope.advancedOptions.showAdvancedOptions = false;
				$scope.advancedOptions.maintainHeaderNaming = false;
			};

			$scope.validateVariateGroupAndSend = function () {
				ImportMappingService.showConfirmIfHasUnmapped().then(function () {
					return ImportMappingService.validateMappingAndSave($scope.advancedOptions.maintainHeaderNaming);
				}, function () {
					return {cancelMapping: true};
				}).then(function (result) {
					setTimeout(function () {

						$('#importMapModal').one('hidden.bs.modal', function () {
						}).modal('hide');

						$rootScope.$broadcast('importObservationAfterMappingVariateGroup');
					}, 300);

					if (result && result.warning) {
						/** @namespace result.warning */
						showAlertMessage('', result.warning);
					}
					$scope.resetAdvancedOptions();
				}, function (failResult) {
					if (failResult.cancelMapping) {
						return;
					}

					var msg = 'Please choose a mapping for all variables that you wish to import in the study.';
					createErrorNotification('Import Observations Mapping Error', msg);

				});

			};

			$scope.launchOntologyBrowser = function () {
				var $importMapModal = $('#importMapModal');
				$importMapModal.one('hidden.bs.modal', function () {
					setTimeout(function () {
						$scope.$apply(function () {
							var title = 'Ontology Browser';
							var url = '/ibpworkbench/controller/ontology';

							$uibModal.open({
								windowClass: 'modal-very-huge',
								controller: 'OntologyBrowserController',
								templateUrl: '/Fieldbook/static/angular-templates/ontologyBrowserPopup.html',
								resolve: {
									title: function () {
										return title;
									},

									url: function () {
										return url;
									}
								}
							}).result.finally(function () {
								// do something after this modal closes
								DesignOntologyService.clearData();

								setTimeout(function () {
									$importMapModal.modal('show');
								}, 200);

							});

						});
					}, 200);

				}).modal('hide');

			};


		}]);

	app.controller('OntologyBrowserController', ['$scope', '$uibModalInstance', 'title', 'url',
		function ($scope, $uibModalInstance, title, url) {
			$scope.title = title;
			$scope.url = url;
			$scope.close = function () {
				$uibModalInstance.dismiss('Cancelled');
			};

		}
	]);

	app.directive('mappingVariateGroup', ['Messages', function (Messages) {
		return {
			restrict: 'E',
			scope: {
				name: '@',
				mappingData: '=data'
			},
			templateUrl: '/Fieldbook/static/angular-templates/importObservations/importMappingGroup.html',
			controller: ['$scope', '$attrs', function ($scope, $attrs) {
				// data structure
				$scope.Messages = Messages;
				$scope.variableType = $attrs.variableType;

				$scope.sortableOptions = {

					connectWith: '.list-group',
					update: function (e, ui) {
						if (!ui.item.sortable.received) {
							var originNgModel = ui.item.sortable.sourceModel;
							var itemModel = originNgModel[ui.item.sortable.index];
							var dropNgModel = ui.item.sortable.droptargetModel;

							var exists = !!dropNgModel.filter(function (item) {
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

				$scope.computeButtonLabel = function (header) {

					if (header.variable) {
						return 'Re-map';
					} else {
						return 'Apply Mapping';
					}
				};

			}]

		};
	}]);

	app.directive('importMapVariableSelection', ['VARIABLE_SELECTION_MODAL_SELECTOR', 'VARIABLE_SELECTED_EVENT_TYPE', 'DesignOntologyService', 'Messages',
		function (VARIABLE_SELECTION_MODAL_SELECTOR, VARIABLE_SELECTED_EVENT_TYPE, DesignOntologyService, Messages) {
			return {
				restrict: 'A',
				scope: {
					modeldata: '=',
					callback: '&',
					mappedheader: '=mappedData'
				},

				link: function (scope, elem, attrs) {
					scope.processData = function (data) {
						scope.$apply(function () {
							if (data.responseData) {
								data = data.responseData;
							}
							if (data) {
								// if retrieved data is an array of values
								if (data.length && data.length > 0) {
									$.each(data, function (key, value) {
										scope.modeldata.id = value.variable.cvTermId;
										scope.modeldata.variable = value.variable;
									});
								}
							}

							$(VARIABLE_SELECTION_MODAL_SELECTOR).modal('hide');
						});
					};

					elem.on('click', function () {
						// temporarily close the current modal
						var $importMapModal = $('#importMapModal');
						var params = {
							variableType: '1043',
							retrieveSelectedVariableFunction: function () {
								return {};
							},
							callback: scope.processData,
							onHideCallback: function () {
								setTimeout(function () {
									$importMapModal.modal('show');
								}, 200);
							},
							apiUrl: '/Fieldbook/manageSettings/settings/role/1043',
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

						$importMapModal.one('hidden.bs.modal', function () {
							setTimeout(function () {
								DesignOntologyService.openVariableSelectionDialog(params);
							}, 200);
						}).modal('hide');

					});
				}
			};
		}
	]);

	app.service('ImportMappingService', ['$http', '$rootScope', '$q', '_', 'Messages', 'datasetService', function ($http, $rootScope, $q,
		_, Messages, datasetService) {

		function validateMappingAndSave(maintainHeaderNaming) {

			var postData = angular.copy(service.data);
			var datasetId = angular.copy(service.datasetId);
			var allMapped = true;
			var deferred = $q.defer();

			delete postData.unmappedHeaders;

			_.forIn(postData, function (value) {
				var variableTypeId;
				var output = [];

				// lets grab all variables that are in groups but does not have mapped variables
				_.forEach(postData, function (value) {
					var results = _.filter(value, function (item) {
						return item.variable === null;
					});

					if (results.length > 0) {
						allMapped = false;

						deferred.reject(results[0].name);

						return false;
					}
				});

				if (!allMapped) {
					showErrorMessage('', 'One or more variables have not been mapped and will not be imported.');
					deferred.reject();
					return;
				}

				$q.all([
					datasetService.getVariables(datasetId, 1807), // selections
					datasetService.getVariables(datasetId, 1808) // traits
				]).then(function (data) {
					_.forEach(data[0], function (selections) {
						output.push(selections.name)
					});
					_.forEach(data[1], function (trait) {
						output.push(trait.name)
					});

					var addVariablesPromises = [];

					for (var i = 0; i < value.length; i++) {

						if (_.has(value[i], 'variable')) {
							if (_.has(value[i].variable, 'id') && value[i].variable.id) {
								value[i].id = value[i].variable.id;
							} else if (_.has(value[i].variable, 'cvTermId') && value[i].variable.cvTermId) {
								value[i].id = value[i].variable.cvTermId;
							} else {
								value[i].id = 0;
							}

							if (!output.includes(value[i].name) && !output.includes(value[i].variable.name)) {
								if (value[i].variable.variableTypes.includes('TRAIT')) {
									variableTypeId = 1808;
								} else {
									variableTypeId = 1807;
								}
								addVariablesPromises.push(datasetService.addVariables(datasetId, {
									variableTypeId: variableTypeId,
									variableId: value[i].variable.id,
									studyAlias: maintainHeaderNaming ? value[i].name : value[i].variable.alias || value[i].variable.name,
								}));
								if(!maintainHeaderNaming) {
									var headerIndex = $rootScope.importedData[0].findIndex(function(name){ return name === value[i].name});
									$rootScope.importedData[0][headerIndex] = value[i].variable.alias || value[i].variable.name;
								}
							} else {
								showErrorMessage('', 'Variable already exists in dataset');
								deferred.reject();
								return;
							}

						}
					}

					$q.all(addVariablesPromises).then(function () {
						deferred.resolve();
					}, function (response) {
						if (response.errors && response.errors.length) {
							showErrorMessage('', response.errors[0].message);
						} else {
							showErrorMessage('', ajaxGenericErrorMsg);
						}
						deferred.reject();
					});
				});
			});

			return deferred.promise;
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
							callback: function () {
								deferred.resolve(true);
							}
						},
						no: {
							label: Messages.NO,
							className: 'btn-default',
							callback: function () {
								deferred.reject(false);
							}
						}
					}
				});
			}

			return deferred.promise;
		}


		var service = {
			data: {
				unmappedHeaders: [],
				mappedTraits: []
			},
			validateMappingAndSave: validateMappingAndSave,
			showConfirmIfHasUnmapped: showConfirmIfHasUnmapped
		};

		return service;

	}]);

	app.service('DesignOntologyService', ['VARIABLE_SELECTION_LABELS', function (VARIABLE_SELECTION_LABELS) {
		var TrialSettingsManager = window.TrialSettingsManager;
		var settingsManager = new TrialSettingsManager(VARIABLE_SELECTION_LABELS);

		return {
			openVariableSelectionDialog: function (params) {
				settingsManager._openVariableSelectionDialog(params);
			},

			// @param = this map contains variables of the pair that will be filtered
			addDynamicFilterObj: function (_map, group) {
				settingsManager._addDynamicFilter(_map, group);
			},
			clearData: function () {
				settingsManager._clearCache();
			}

		};
	}]);
})();

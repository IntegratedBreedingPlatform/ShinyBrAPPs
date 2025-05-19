/*global angular, showAlertMessage, showErrorMessage*/
(function () {
	'use strict';

	var derivedVariableModule = angular.module('derived-variable', ['ui.bootstrap', 'datasets-api', 'datasetOptionModal', 'fieldbook-utils']);

	derivedVariableModule.factory('derivedVariableService', ['$http', '$q', 'studyContext', 'datasetService',
		'VARIABLE_TYPES', 'DATASET_TYPES_OBSERVATION_IDS',
		function ($http, $q, studyContext, datasetService, VARIABLE_TYPES, DATASET_TYPES_OBSERVATION_IDS) {

			var derivedVariableService = {};

			var FIELDBOOK_BASE_URL = '/Fieldbook/DerivedVariableController/';
			var BMSAPI_BASE_URL = '/bmsapi/crops/' + studyContext.cropName + '/programs/' + studyContext.programId + '/studies/';

			derivedVariableService.isStudyHasCalculatedVariables = true;

			var successHandler = function (response) {

				if (response.data && response.data.inputMissingData) {
					showAlertMessage('', response.data.inputMissingData, 15000);
				}
				return response;

			};
			var failureHandler = function (response) {
				if (response.data.errorMessage) {
					showErrorMessage('', response.data.errorMessage);
				} else if (response.data.errors) {
					showErrorMessage('', response.data.errors[0].message);
				} else {
					showErrorMessage('', ajaxGenericErrorMsg);
				}
			};

			derivedVariableService.hasMeasurementData = function (variableIds) {
				return $http.post(FIELDBOOK_BASE_URL + 'derived-variable/dependencyVariableHasMeasurementData/',
					variableIds, {cache: false});
			};

			derivedVariableService.calculateVariableForSubObservation = function (datasetId, calculateData) {
				var request = $http.post(BMSAPI_BASE_URL + studyContext.studyId + '/datasets/' + datasetId + '/derived-variables/calculation', calculateData);
				return request.then(successHandler, failureHandler);
			};

			derivedVariableService.getMissingFormulaVariables = function (datasetId, variableId) {
				if (!studyContext.studyId) {
					return $q.resolve();
				}
				return $http.get(BMSAPI_BASE_URL + studyContext.studyId + '/datasets/' + datasetId + '/derived-variables/' + variableId + '/formula-variables/missing');
			};

			derivedVariableService.getFormulaVariableDatasetMap = function (datasetId, variableId) {
				if (!studyContext.studyId) {
					return $q.resolve();
				}
				return $http.get(BMSAPI_BASE_URL + studyContext.studyId + '/datasets/' + datasetId + '/derived-variables/' + variableId + '/formula-variables/dataset-map');
			};

			derivedVariableService.getFormulaVariables = function (datasetId) {
				if (!studyContext.studyId) {
					return $q.resolve();
				}
				return $http.get(BMSAPI_BASE_URL + studyContext.studyId + '/datasets/' + datasetId + '/derived-variables/formula-variables');
			};

			derivedVariableService.countCalculatedVariables = function (datasetIds) {
				var request = $http.head(BMSAPI_BASE_URL + studyContext.studyId + '/derived-variables', {
					params: {
						datasetIds: datasetIds.join(",")
					}
				});
				return request.then(successHandler, failureHandler);
			};

			/**
			 * Displays the Execute Calculated Variable action menu if any of the datasets (plot and sub-observations) in the current study has calculated traits.
			 * If none of the datasets have calculated variables, Execute Calculated Variable action menu is hidden.
			 *
			 * This method is triggered when:
			 * 1. A study is opened and initialized.
			 * 2. Measurement table's trait section is changed (added/removed) and is saved.
			 * 3. SubObservation trait is changed (added/removed)
			 */
			derivedVariableService.displayExecuteCalculateVariableMenu = function () {

				if (!studyContext.studyId) return;

				datasetService.getDatasets(DATASET_TYPES_OBSERVATION_IDS).then(function (datasets) {
					var datasetIds = [];
					datasets.forEach(function (dataset) {
						datasetIds.push(dataset.datasetId)
					});

					if (datasetIds.length === 0) {
						derivedVariableService.isStudyHasCalculatedVariables = false;
						return;
					}

					derivedVariableService.countCalculatedVariables(datasetIds).then(function (response) {
						var count = response.headers('X-Total-Count');
						var hasCalculatedVariable = parseInt(count) > 0;
						// isStudyHasCalculatedVariables variable is bound to HTML element via ngShow directive.
						derivedVariableService.isStudyHasCalculatedVariables = hasCalculatedVariable;
					});
				});

			};

			derivedVariableService.showWarningIfDependenciesAreMissing = function (datasetId, variableId) {
				derivedVariableService.getMissingFormulaVariables(datasetId, variableId).then(function (response) {
					var missingFormulaVariables = response.data;
					if (missingFormulaVariables.length > 0) {
						var missingFormulaVariablesNames = [];
						angular.forEach(missingFormulaVariables, function (formulaVariable) {
							missingFormulaVariablesNames.push(formulaVariable.name);
						});
						showAlertMessage('', 'The variable(s) ' + missingFormulaVariablesNames.join(', ') + ' are not included in this study. ' +
							'You will need data for these variables to calculate values for this variable.', 15000);
					}
				});
			};

			derivedVariableService.showWarningIfCalculatedVariablesAreOutOfSync = function () {
				datasetService.getDatasets().then(function (data) {
					var datasetsWithOutOfSyncData = [];
					angular.forEach(data, function (dataset) {
						if (dataset.hasOutOfSyncData) {
							datasetsWithOutOfSyncData.push(dataset.name);
						}
					});
					if (datasetsWithOutOfSyncData.length > 0) {
						showAlertMessage('', 'Some of the calculated variable data in the dataset(s) ' + datasetsWithOutOfSyncData.join(', ') + ' is out of sync ' +
							'because the input data has changed. Please recalculate the variable to update the results.', 15000);
					}
				});
			}

			return derivedVariableService;

		}]);


	derivedVariableModule.factory('derivedVariableModalService', ['$uibModal', function ($uibModal) {

		var derivedVariableModalService = {};

		derivedVariableModalService.openDatasetOptionModal = function () {
			$uibModal.open({
				template: '<dataset-option-modal modal-title="modalTitle" message="message"' +
					'selected="selected" on-continue="next()"></dataset-option-modal>',
				controller: 'executeCalculatedVariableDatasetOptionCtrl',
				size: 'md'
			});
		};

		derivedVariableModalService.openExecuteCalculatedVariableModal = function (datasetId) {
			$uibModal.open({
				templateUrl: '/Fieldbook/static/angular-templates/derivedVariable/executeCalculatedVariableModal.html',
				controller: "executeCalculatedVariableModalCtrl",
				size: 'md',
				resolve: {
					datasetId: function () {
						return datasetId;
					}
				},
				controllerAs: 'ctrl'
			});
		};

		derivedVariableModalService.confirmOverrideCalculatedVariableModal = function (datasetId, selectedVariable, calculateRequestData) {
			$uibModal.open({
				templateUrl: '/Fieldbook/static/angular-templates/derivedVariable/confirmOverrideCalculatedVariableModal.html',
				controller: "confirmOverrideCalculatedVariableModalCtrl",
				size: 'md',
				resolve: {
					datasetId: function () {
						return datasetId;
					},
					selectedVariable: function () {
						return selectedVariable;
					},
					calculateRequestData: calculateRequestData
				},
				controllerAs: 'ctrl'
			});
		};


		derivedVariableModalService.selectDatasetPerInputVariableModal = function (datasetId, calculateRequestData, selectedVariable, inputVariableDatasetMap) {
			$uibModal.open({
				templateUrl: '/Fieldbook/static/angular-templates/derivedVariable/selectDatasetPerInputVariableModal.html',
				controller: "selectDatasetPerInputVariableModalCtrl",
				size: 'md',
				resolve: {
					datasetId: function () {
						return datasetId;
					},
					inputVariableDatasetMap: function () {
						return inputVariableDatasetMap;
					},
					selectedVariable: function () {
						return selectedVariable;
					},
					calculateRequestData: calculateRequestData
				},
				controllerAs: 'ctrl'
			});
		};

		return derivedVariableModalService;

	}]);

	derivedVariableModule.controller('executeCalculatedVariableDatasetOptionCtrl', ['$rootScope', '$scope', '$uibModal', '$uibModalInstance',
		'studyContext', 'derivedVariableModalService', 'derivedVariableService',
		function ($rootScope, $scope, $uibModal, $uibModalInstance, studyContext, derivedVariableModalService, derivedVariableService) {

			$scope.modalTitle = 'Execute Calculations';
			$scope.message = 'Please choose the dataset where you would like to execute the calculation from:';
			$scope.measurementDatasetId = studyContext.measurementDatasetId;
			$scope.selected = {datasetId: $scope.measurementDatasetId};

			$scope.next = function () {
				$rootScope.navigateToSubObsTab($scope.selected.datasetId, {isPendingView: false, reload: true});
				derivedVariableModalService.openExecuteCalculatedVariableModal($scope.selected.datasetId);
				$uibModalInstance.close();
			};

		}]);

	derivedVariableModule.controller('executeCalculatedVariableModalCtrl',
		['$q', '$rootScope', '$scope', '$http', '$uibModalInstance', 'datasetService', 'derivedVariableModalService', 'derivedVariableService', 'datasetId', 'studyContext',
			function ($q, $rootScope, $scope, $http, $uibModalInstance, datasetService, derivedVariableModalService, derivedVariableService, datasetId, studyContext) {

				$scope.instances = [];
				$scope.selectedInstances = {};
				$scope.isEmptySelection = false;
				$scope.selected = {variable: undefined};

				$scope.init = function () {
					datasetService.getDataset(datasetId).then(function (dataset) {
						$scope.variableListView = buildVariableListView(dataset.variables);
						$scope.instances = dataset.instances;
						if ($scope.variableListView.length === 0) {
							showErrorMessage('', 'There is no calculated variable for the dataset selected. Please add a calculated variable in the dataset and try again.');
						}
					});
				};

				$scope.cancel = function () {
					$uibModalInstance.close();
				};

				$scope.reloadSubObservation = function () {
					$rootScope.navigateToSubObsTab(datasetId, {isPendingView: false, reload: true});
					showSuccessfulMessage('', 'Calculated values for ' + $scope.selected.variable.name + ' were added successfully.');
				};

				$scope.execute = function () {
					var geoLocationIds = [];

					Object.keys($scope.selectedInstances).forEach(function (instanceDbId) {
						var isSelected = $scope.selectedInstances[instanceDbId];
						if (isSelected) {
							geoLocationIds.push(instanceDbId);
						}
					});

					var calculateRequestData = {
						variableId: $scope.selected.variable.cvTermId
						, geoLocationIds: geoLocationIds
						, inputVariableDatasetMap: {}
					};


					derivedVariableService.getFormulaVariableDatasetMap(datasetId, $scope.selected.variable.cvTermId).then(function (response) {

						if (response.data.length !== 0) {

							var hasMultipleOccurrenceInDataset = false;

							angular.forEach(response.data, function (value, key) {
								// initialize inputVariableDatasetMap in calculateRequestData
								calculateRequestData.inputVariableDatasetMap[key] = value.datasets[0].id;
								if (value.datasets.length > 1) {
									hasMultipleOccurrenceInDataset = true;
								}
							});

							if (hasMultipleOccurrenceInDataset) {
								derivedVariableModalService.selectDatasetPerInputVariableModal(datasetId, calculateRequestData, $scope.selected.variable, response.data);
								$uibModalInstance.close();
								return $q.reject();
							}
						}
					}).then(function () {
						derivedVariableService.calculateVariableForSubObservation(datasetId, calculateRequestData)
							.then(function (response) {
								if (response) {
									if (response.data && response.data.hasDataOverwrite) {
										derivedVariableModalService.confirmOverrideCalculatedVariableModal(datasetId, $scope.selected.variable, calculateRequestData);
									} else {
										$scope.reloadSubObservation();
									}
								}
								$uibModalInstance.close();
							});
					});

				};

				function buildVariableListView(variables) {
					var variableListView = [];
					angular.forEach(variables, function (variable) {
						if (variable.formula) {
							variableListView.push({name: variable.name, cvTermId: variable.termId});//termId
						}
					});
					return variableListView;
				};


				$scope.init();

			}]);

	derivedVariableModule.controller('confirmOverrideCalculatedVariableModalCtrl', ['$rootScope', '$scope', '$http', '$uibModalInstance',
		'derivedVariableModalService', 'derivedVariableService', 'selectedVariable', 'datasetId', 'studyContext', 'calculateRequestData',
		function ($rootScope, $scope, $http, $uibModalInstance, derivedVariableModalService, derivedVariableService, selectedVariable,
				  datasetId, studyContext, calculateRequestData) {

			$scope.goBack = function () {
				$http.get('/Fieldbook/ImportManager/revert/data')
					.then(function (response) {

						if (datasetId === studyContext.measurementDatasetId) {
							// Only revert plot measurements data if the selected dataset is PLOT DATA
							$scope.revertData();
						}
						$uibModalInstance.close();
						derivedVariableModalService.openExecuteCalculatedVariableModal(datasetId);
					});

			};

			$scope.revertData = function () {
				$('body').removeClass('import-preview-measurements');
				showSuccessfulMessage('', 'Discarded data successfully');

				if ($('#measurement-table').length !== 0 && $('#measurement-table').dataTable()) {
					$('#measurement-table').dataTable().fnAdjustColumnSizing();
				}
				$('#review-out-of-bounds-data-list').hide();
				$('.fbk-discard-imported-data').addClass('fbk-hide');
				$('.import-study-data').data('data-import', '0');
			};

			$scope.proceed = function () {

				// Explicitly tell the web service to save the calculated value immediately even if there's measurement data to overwrite.
				calculateRequestData.overwriteExistingData = true;
				derivedVariableService.calculateVariableForSubObservation(datasetId, calculateRequestData)
					.then(function (response) {
						if (response) {
							$rootScope.navigateToSubObsTab(datasetId, {isPendingView: false, reload: true});
							showSuccessfulMessage('', 'Calculated values for ' + selectedVariable.name + ' were added successfully.');
							$uibModalInstance.close();
						}
					});

				$uibModalInstance.close();

			};

		}]);

	derivedVariableModule.controller('selectDatasetPerInputVariableModalCtrl', ['$rootScope', '$scope', '$uibModal', '$uibModalInstance',
		'studyContext', 'derivedVariableModalService', 'derivedVariableService', 'datasetId', 'calculateRequestData', 'inputVariableDatasetMap',
		'selectedVariable',
		function ($rootScope, $scope, $uibModal, $uibModalInstance, studyContext, derivedVariableModalService, derivedVariableService, datasetId,
				  calculateRequestData, inputVariableDatasetMap, selectedVariable) {

			$scope.calculateRequestData = calculateRequestData;
			$scope.inputVariableDatasetMap = inputVariableDatasetMap;
			$scope.inputVariablesWithMultipleDataset = getInputVariablesWithMultipleDataset(inputVariableDatasetMap);

			$scope.proceed = function () {
				derivedVariableService.calculateVariableForSubObservation(datasetId, $scope.calculateRequestData)
					.then(function (response) {
						if (response) {
							if (response.data && response.data.hasDataOverwrite) {
								derivedVariableModalService.confirmOverrideCalculatedVariableModal(datasetId, selectedVariable, calculateRequestData);
							} else {
								$scope.reloadSubObservation();
							}
						}
						$uibModalInstance.close();
					});
			};

			$scope.cancel = function () {
				$uibModalInstance.close();
			};

			$scope.reloadSubObservation = function () {
				$rootScope.navigateToSubObsTab(datasetId, {isPendingView: false, reload: true});
				showSuccessfulMessage('', 'Calculated values for ' + selectedVariable.name + ' were added successfully.');
			};

			function getInputVariablesWithMultipleDataset(inputVariableDatasetMap) {
				var inputVariableNames = [];
				angular.forEach(inputVariableDatasetMap, function (item) {
					if (item.datasets.length > 1) {
						inputVariableNames.push(item.variableName);//termId
					}
				});
				return inputVariableNames;
			}

		}]);
})();

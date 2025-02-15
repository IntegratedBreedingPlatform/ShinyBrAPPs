(function () {
	'use strict';

	var subObsTabSelectedDeRegister = () => {
	};
	var sampleListCreatedDeRegister = () => {
	};
	var startPlantingPreparationDeRegister = () => {
	};
	var changingPlotEntryDeRegister = () => {
	};

	var subObservationModule = angular.module('subObservation', ['visualization']);
	var TRIAL_INSTANCE = 8170,
		GID = 8240,
		GROUPGID = 8330,
		LINE_GID = 8836,
		TESTER_GID = 8839,
		OBS_UNIT_ID = 8201;
	var SAMPLES = -2;
	var STOCK_ID = -1727;
	var hiddenColumns = [OBS_UNIT_ID, TRIAL_INSTANCE];

	subObservationModule.controller('SubObservationSetCtrl', ['$scope', '$rootScope', 'TrialManagerDataService', '$stateParams',
		'DTOptionsBuilder', 'DTColumnBuilder', '$http', '$q', '$compile', 'studyInstanceService', 'datasetService', 'derivedVariableService', '$timeout', '$uibModal',
		'visualizationModalService', 'studyContext',
		function ($scope, $rootScope, TrialManagerDataService, $stateParams, DTOptionsBuilder, DTColumnBuilder, $http, $q, $compile,
				  studyInstanceService, datasetService, derivedVariableService, $timeout, $uibModal, visualizationModalService, studyContext
		) {

			// used also in tests - to call $rootScope.$apply()
			var tableLoadedResolve;
			$scope.tableLoadedPromise = new Promise(function (resolve) {
				tableLoadedResolve = resolve;
			});

			var tableRenderedResolve;
			$scope.tableRenderedPromise = new Promise(function (resolve) {
				tableRenderedResolve = resolve;
			});

			$scope.hasInstances = false;

			$scope.toggleSectionBatchAction = false;
			$scope.hasVariableFilter = false;
			$scope.selectVariableFilter = [];
			$scope.traitVariables = new angular.OrderedHash();
			$scope.selectionVariables = new angular.OrderedHash();
			$scope.isHideDelete = false;
			$scope.addVariable = true;
			var subObservationSet = $scope.subObservationSet = $stateParams.subObservationSet;
			$scope.columnsObj = subObservationSet.columnsObj;
			$scope.rows = subObservationSet.rows;
			$scope.nested = {};
			$scope.nested.dtInstance = null;
			$scope.nested.reviewVariable = null;
			$scope.nested.selectedVariableFilter = null;
			$scope.nested.selectedBatchAction = null;
			$scope.nested.newValueBatchUpdate = null;
			$scope.enableActions = false;
			$scope.isCategoricalDescriptionView = window.isCategoricalDescriptionView;

			var subObservationTab = $scope.subObservationTab;
			var tableId = '#subobservation-table-' + subObservationTab.id + '-' + subObservationSet.id;
			var dtColumnsPromise = $q.defer();
			var dtColumnDefsPromise = $q.defer();

			$scope.dtColumns = dtColumnsPromise.promise;
			$scope.dtColumnDefs = dtColumnDefsPromise.promise;
			$scope.dtOptions = null;

			$scope.totalItems = 0;
			$scope.selectedItems = [];
			$scope.isAllPagesSelected = false;

			$scope.columnFilter = {
				selectAll: function () {
					this.columnData.possibleValues.forEach(function (value) {
						value.isSelectedInFilters = this.columnData.isSelectAll;
					}.bind(this));
				},
				selectOption: function (selected) {
					if (!selected) {
						this.columnData.isSelectAll = false;
					}
				},
				search: function (item) {
					var query = $scope.columnFilter.columnData.query;
					if (!query) {
						return true;
					}
					if (item.name.indexOf(query) !== -1 || item.displayDescription.indexOf(query) !== -1) {
						return true;
					}
					return false;
				}
			};
			$scope.datepickerOptions = {
				showWeeks: false
			};
			$scope.selectedStatusFilter = "1";

			// Define which elements trigger this menu: only non-empty, non-disabled categorical or numeric traits
			var contextMenuSelector =
				"#subObservationTableContainer td.variates.datatype-1130:not(:empty):not([disabled])," +
				"#subObservationTableContainer td.variates.datatype-1110:not(:empty):not([disabled])";
			$.contextMenu('destroy', contextMenuSelector);

			$.contextMenu({
				selector: contextMenuSelector,
				// define the elements of the menu
				callback: function (key, opt) {
					var cell = opt.$trigger.get(0);
					var dtCell = table().cell(cell);
					var cellData = dtCell.data();
					var dtRow = table().row(cell.parentNode);
					var rowData = dtRow.data();

					var newValue, newDraftValue, newDraftCategoricalValueId;

					switch (key) {
						case 'accept':
							newDraftValue = newDraftCategoricalValueId = null;
							newValue = cellData.draftValue;
							break;
						case 'missing':
							newValue = 'missing';
							if ($scope.isPendingView) {
								newDraftValue = newDraftCategoricalValueId = null;
							} else {
								newDraftValue = cellData.draftValue;
								newDraftCategoricalValueId = cellData.draftCategoricalValueId;
							}
							break;
					}

					var index = table().colReorder.transpose(table().column(cell).index(), 'toOriginal');
					var columnData = $scope.columnsObj.columns[index].columnData;
					datasetService.updateObservation(subObservationSet.id, rowData.observationUnitId, cellData.observationId, {
							categoricalValueId: getCategoricalValueId(newValue, columnData),
							value: newValue,
							draftValue: newDraftValue,
							draftCategoricalValueId: newDraftCategoricalValueId
						}
					).then(function () {
						if (table().data().length == 1 && $scope.isPendingView) {
							datasetService.getDataset(subObservationSet.id).then(function (dataset) {
								if (!dataset.hasPendingData) {
									reloadDataset();
								} else {
									table().ajax.reload(null, false)
								}
							}, function (response) {
								if (response.errors && response.errors.length) {
									showErrorMessage('', response.errors[0].message);
								} else {
									showErrorMessage('', ajaxGenericErrorMsg);
								}
							});
						} else {
							table().ajax.reload(null, false)
						}
					}, function (response) {
						if (response.errors && response.errors.length) {
							showErrorMessage('', response.errors[0].message);
						} else {
							showErrorMessage('', ajaxGenericErrorMsg);
						}
					});

				},
				items: {
					accept: {
						name: "Accept value as-is", visible: function () {
							return $scope.isPendingView;
						}
					},
					missing: {
						name: "Set value to missing"
					}
				}
			});

			datasetService.getDataset(subObservationSet.id).then(function (dataset) {
				$scope.subObservationSet.dataset = dataset;
				$scope.traitVariables = $scope.getVariables('TRAIT');
				$scope.selectionVariables = $scope.getVariables('SELECTION_METHOD');
				$scope.selectedVariables = $scope.getSelectedVariables();

				if (!dataset.instances || !dataset.instances.length) {
					$scope.hasInstances = false;
					return;
				}

				$scope.hasInstances = true;
				$scope.environments = [{
					instanceNumber: null,
					locationName: 'All environments'
				}].concat(dataset.instances);

				subObservationSet.hasPendingData = subObservationTab.hasPendingData = dataset.hasPendingData;
				// we set pending view unless we are specifically told not to
				$scope.isPendingView = dataset.hasPendingData && $stateParams.isPendingView !== false;
				doPendingViewActions();

				loadTable();
			}); // getDataset

			subObsTabSelectedDeRegister();
			subObsTabSelectedDeRegister = $rootScope.$on('subObsTabSelected', function (event) {
				adjustColumns();
			});

			sampleListCreatedDeRegister();
			sampleListCreatedDeRegister = $rootScope.$on('sampleListCreated', function (event) {
				loadTable();
			});

			startPlantingPreparationDeRegister();
			startPlantingPreparationDeRegister = $rootScope.$on('startPlantingPreparation', function (event) {
				$scope.tableRenderedPromise.then(function () {
					if (!$scope.validateSelection()) {
						return;
					}
					$uibModal.open({
						templateUrl: '/Fieldbook/static/js/trialmanager/inventory/planting-preparation/planting-preparation-modal.html',
						windowClass: 'modal-very-huge',
						controller: 'PlantingPreparationModalCtrl',
						resolve: {
							searchComposite: function () {
								return {
									itemIds: $scope.selectedItems.length ? $scope.selectedItems : null,
									searchRequest: $scope.selectedItems.length ? null : {
										instanceId: $scope.nested.selectedEnvironment.instanceId,
										draftMode: $scope.isPendingView,
										filter: getFilter()
									}
								};
							},
							datasetId: function () {
								return $scope.subObservationSet.dataset.datasetId;
							}
						}
					}).result.then(() => {
						loadTable();
					});
				});
			});

			changingPlotEntryDeRegister();
			changingPlotEntryDeRegister = $rootScope.$on('changePlotEntry', function (event) {
				$scope.tableRenderedPromise.then(function () {
					if (!$scope.validateSelection()) {
						return;
					}
					var searchComposite = {
						itemIds: $scope.selectedItems.length ? $scope.selectedItems : null,
						searchRequest: $scope.selectedItems.length ? null : {
							instanceId: $scope.nested.selectedEnvironment.instanceId,
							draftMode: $scope.isPendingView,
							filter: getFilter()
						}
					};

					datasetService.getObservationUnitsMetadata(searchComposite, $scope.subObservationSet.dataset.datasetId).then(function (response) {
						$uibModal.open({
							templateUrl: '/Fieldbook/static/js/trialmanager/observations/change-plot-entry-modal.html',
							windowClass: 'modal-very-huge',
							controller: 'ChangePlotEntryModalCtrl',
							resolve: {
								searchComposite: function () {
									return searchComposite;
								},
								datasetId: function () {
									return $scope.subObservationSet.dataset.datasetId;
								},
								numberOfInstances: function () {
									return response.instancesCount;
								},
								numberOfPlots: function () {
									return response.observationUnitsCount;
								},
							}
						}).result.then(() => {
							loadTable();
						});
					});
				});
			});

			$scope.getVariables = function (variableType) {
				var variables = {settings: []};
				angular.forEach($scope.subObservationSet.dataset.variables, function (datasetVariable) {
					var SettingDetail = $scope.transformSettingDetails(datasetVariable, variableType);
					variables.settings.push(SettingDetail);

				});
				return TrialManagerDataService.extractSettings(variables);
			};

			$scope.transformSettingDetails = function (datasetVariable, variableType) {
				var variable = $scope.transformVariable(datasetVariable);
				var SettingDetail = {
					variable: variable,
					hidden: datasetVariable.variableType != variableType,
					deletable: true
				};
				return SettingDetail;
			};

			$scope.transformVariable = function (datasetVariable) {
				var variable = {
					cvTermId: datasetVariable.termId,
					name: datasetVariable.alias,
					description: datasetVariable.description,
					property: datasetVariable.property,
					scale: datasetVariable.scale,
					//role:null,
					method: datasetVariable.method,
					dataType: datasetVariable.dataType,
					dataTypeId: datasetVariable.dataTypeId,
					minRange: null,
					maxRange: null,
					operation: null,
					formula: datasetVariable.formula
				};
				return variable;
			};

			$scope.getSelectedVariables = function () {
				var selected = {};
				angular.forEach($scope.subObservationSet.dataset.variables, function (variable) {
					selected[variable.termId] = variable.alias;
				});
				return selected;
			};

			$scope.selectVariableCallback = function (responseData) {
				// just override default callback (see VariableSelection.prototype._selectVariable)
			};

			$scope.onHideCallback = function () {
				adjustColumns();
			};

			$scope.onAddVariable = function (result, variableTypeId) {
				var variable = undefined;
				angular.forEach(result, function (val) {
					variable = val.variable;
					val.deletable = true;
					variable.description = variable.definition;
					variable.name = variable.alias ? variable.alias : variable.name;
					variable.termId = variable.id;
				});

				datasetService.addVariables($scope.subObservationSet.dataset.datasetId, {
					variableTypeId: variableTypeId,
					variableId: variable.id,
					studyAlias: variable.name
				}).then(function () {
					$scope.subObservationSet.dataset.variables.push(variable);
					if ($scope.hasInstances) {
						loadTable();
						derivedVariableService.displayExecuteCalculateVariableMenu();
						derivedVariableService.showWarningIfDependenciesAreMissing($scope.subObservationSet.dataset.datasetId, variable.id);
					}
				}, function (response) {
					if (response.errors && response.errors.length) {
						showErrorMessage('', response.errors[0].message);
					} else {
						showErrorMessage('', ajaxGenericErrorMsg);
					}
				});
			};

			$scope.onRemoveVariable = function (variableIds, settings) {

				$scope.checkVariableIsUsedInCalculatedVariable(variableIds).then(function (isVariableUsedInOtherCalculatedVariable) {
					return $scope.checkVariableHasMeasurementData(isVariableUsedInOtherCalculatedVariable, variableIds);
				}).then(function (doContinue) {
					if (doContinue) {
						datasetService.removeVariables($scope.subObservationSet.dataset.datasetId, variableIds).then(function () {
							reloadDataset();
							derivedVariableService.displayExecuteCalculateVariableMenu();
						}, function (response) {
							if (response.errors && response.errors.length) {
								showErrorMessage('', response.errors[0].message);
							} else {
								showErrorMessage('', ajaxGenericErrorMsg);
							}
						});
					}
				});
			};

			$scope.checkVariableHasMeasurementData = function (isVariableUsedInOtherCalculatedVariable, deleteVariables) {
				var deferred = $q.defer();
				if (deleteVariables.length != 0) {
					datasetService.observationCount($scope.subObservationSet.dataset.datasetId, deleteVariables).then(function (response) {
						var count = response.headers('X-Total-Count');
						if (count > 0) {
							var message = isVariableUsedInOtherCalculatedVariable ? removeVariableDependencyConfirmationText : observationVariableDeleteConfirmationText;
							var modalInstance = $scope.openConfirmModal(message, environmentConfirmLabel);
							modalInstance.result.then(deferred.resolve);
						} else {
							deferred.resolve(true);
						}
					});
				}
				return deferred.promise;
			};

			$scope.checkVariableIsUsedInCalculatedVariable = function (deleteVariables) {
				var deferred = $q.defer();
				var variableIsUsedInOtherCalculatedVariable = false;

				// Retrieve all formula variables in study
				derivedVariableService.getFormulaVariables($scope.subObservationSet.dataset.datasetId).then(function (response) {
					var formulaVariables = response.data;
					// Check if any of the deleted variables are formula variables
					angular.forEach(formulaVariables, function (formulaVariable) {
						if (deleteVariables.indexOf(formulaVariable.id) > -1) {
							variableIsUsedInOtherCalculatedVariable = true;
						}
					});
					deferred.resolve(variableIsUsedInOtherCalculatedVariable);

				});
				return deferred.promise;
			};

			$scope.validateApplyBatchAction = function (messages) {
				var deferred = $q.defer();
				var confirmApplyAction = "Proceed";
				var modalConfirmationBatch = $scope.openConfirmModal(messages, confirmApplyAction);
				modalConfirmationBatch.result.then(deferred.resolve);
				return deferred.promise;
			};

			$scope.togglePendingView = function (isPendingView) {
				if ($scope.isPendingView === isPendingView) {
					return;
				}
				$scope.isPendingView = isPendingView;
				$scope.selectedStatusFilter = "1";
				doPendingViewActions();
				resetChecksStatus();
				loadTable();
			};

			$scope.collapseBatchAction = function () {
				$scope.toggleSectionBatchAction = !$scope.toggleSectionBatchAction;
			};

			$scope.checkOutOfBoundDraftData = function () {
				var deferred = $q.defer();

				datasetService.checkOutOfBoundDraftData($scope.subObservationSet.dataset.datasetId).then(function (response) {
					var modalInstance = $scope.openAcceptPendingModal();
					modalInstance.result.then(deferred.resolve);
				}, function (response) {
					if (response.status == 404) {
						deferred.resolve(true);
					} else if (response.errors && response.errors.length) {
						showErrorMessage('', response.data.errors[0].message);
					} else {
						showErrorMessage('', ajaxGenericErrorMsg);
					}
				});

				return deferred.promise;
			};

			$scope.acceptDraftData = function () {
				$scope.checkOutOfBoundDraftData().then(function (result) {
					var promise;

					if (result === true || result === "1") {
						promise = datasetService.acceptDraftData($scope.subObservationSet.dataset.datasetId).then();
					} else if (result === "2") {
						promise = datasetService.setAsMissingDraftData($scope.subObservationSet.dataset.datasetId).then();
					}
					if (promise) {
						promise.then(function () {
							reloadDataset();
							derivedVariableService.showWarningIfCalculatedVariablesAreOutOfSync();
						}, function (response) {
							if (response.errors && response.errors.length) {
								showErrorMessage('', response.errors[0].message);
							} else {
								showErrorMessage('', ajaxGenericErrorMsg);
							}
						});
					}
				});
			};

			$scope.rejectDraftData = function () {
				var confirmModal = $scope.openConfirmModal(importDiscardDataWarningMessage);
				confirmModal.result.then(function (doContinue) {
					if (doContinue) {
						datasetService.rejectDraftData($scope.subObservationSet.dataset.datasetId).then(function () {
							reloadDataset();
						}, function (response) {
							if (response.errors && response.errors.length) {
								showErrorMessage('', response.errors[0].message);
							} else {
								showErrorMessage('', ajaxGenericErrorMsg);
							}
						});
					}
				});
			};

			$scope.openAcceptPendingModal = function () {
				return $uibModal.open({
					templateUrl: '/Fieldbook/static/angular-templates/subObservations/acceptPendingModal.html',
					controller: function ($scope, $uibModalInstance) {
						$scope.selected = "1";

						$scope.proceed = function () {
							$uibModalInstance.close($scope.selected);
						};
						$scope.cancel = function () {
							$uibModalInstance.close(null);
						};
					}
				});
			};

			$scope.subDivide = function () {
				// TODO
				// var id = $scope.subObservationTab.subObservationSets.length + 1;
				// var name = 'Sub-observation set ' + $scope.subObservationTab.id + ' - subObservationSet ' + id;
				// $scope.subObservationTab.subObservationSets.push({
				// 	id: id,
				// 	name: name
				// });
			};

			$scope.changeEnvironment = function () {
				table().columns("TRIAL_INSTANCE:name").visible($scope.nested.selectedEnvironment === $scope.environments[0]);
				resetChecksStatus();
				table().ajax.reload();
			};

			$scope.changeStatusFilter = function () {
				resetChecksStatus();
				table().ajax.reload();
			};

			$scope.toggleShowCategoricalDescription = function () {
				switchCategoricalView().done(function () {
					$('.fbk-measurement-categorical-name').toggle();
					$('.fbk-measurement-categorical-desc').toggle();

					$scope.$apply(function () {
						$scope.isCategoricalDescriptionView = window.isCategoricalDescriptionView;
						adjustColumns();
					});
				});
			};

			$scope.openColumnFilter = function (index) {
				$scope.columnFilter.index = index;
				$scope.columnFilter.columnData = $scope.columnsObj.columns[index].columnData;
				if ($scope.columnFilter.columnData.sortingAsc != null && !table().order().some(function (order) {
					return order[0] === index;
				})) {
					$scope.columnFilter.columnData.sortingAsc = null;
				}
			};

			function loadBatchActionCombo() {
				$scope.toggleSectionBatchAction = false;
				$scope.selectBatchActions = [{
					name: 'Select action',
					id: null
				}, {
					name: 'Apply new value to observations',
					id: 1
				}];
				$scope.nested.selectedVariableFilter = $scope.selectVariableFilter[0];
				$scope.nested.selectedBatchAction = $scope.selectBatchActions[0];
				if ($scope.isPendingView) {
					$scope.selectBatchActions.push({
						name: 'Accept observations as-is',
						id: 2
					});
				} else {

				}
			};

			$scope.changeSelectedVariableFilter = function () {
				table().ajax.reload();
				$scope.nested.newValueBatchUpdate = null;
				$scope.nested.selectedBatchAction = $scope.selectBatchActions[0];
			};

			$scope.hasDataFiltered = function () {
				return $scope.nested.dtInstance && //
					$scope.nested.dtInstance.DataTable && //
					$scope.nested.dtInstance.DataTable.data() && //
					$scope.nested.dtInstance.DataTable.data().length > 0 || false;
			};

			function newValueBatchUpdateValidation() {
				if ($scope.nested.selectedVariableFilter.dataTypeCode === 'D') {
					var newValue = $.datepicker.formatDate("yymmdd", $scope.nested.newValueBatchUpdate);
					// FIXME find a better way
					if (!newValue) {
						showErrorMessage('', 'Invalid value.');
						return false;
					}
				}

				if ($scope.nested.selectedVariableFilter.dataTypeCode === 'N') {
					var newValue = $scope.nested.newValueBatchUpdate;
					if ($scope.nested.selectedVariableFilter.minRange && $scope.nested.selectedVariableFilter.maxRange && //
						(newValue < $scope.nested.selectedVariableFilter.minRange || newValue > $scope.nested.selectedVariableFilter.maxRange)) {
						showAlertMessage('', 'New value is out of range.');
					}
					return true;
				}
				return true;
			}

			$scope.disableApply = function () {
				return $scope.nested.selectedBatchAction.id === 1 && ($scope.nested.newValueBatchUpdate === null || $scope.nested.newValueBatchUpdate === '')
			};

			$scope.applyBatchAction = function () {

				if ($scope.nested.selectedBatchAction.id === 1) {
					var validated = newValueBatchUpdateValidation();
					if (!validated) {
						return;
					}
				}

				var param = JSON.stringify({
					instanceId: $scope.nested.selectedEnvironment.instanceId,
					draftMode: $scope.isPendingView,
					filter: getFilter()
				});
				datasetService.countFilteredPhenotypesAndInstances(subObservationSet.id, param).then(function (response) {
					var messages = "This action will update " + response.totalFilteredPhenotypes + " observations in "
						+ response.totalFilteredInstances + " environments. You will not be able to undo this transaction." +
						" Are you sure you want to proceed?";
					$scope.validateApplyBatchAction(messages).then(function (doContinue) {
						if (!doContinue) {
							return;
						}
						switch ($scope.nested.selectedBatchAction.id) {
							case 1:
								// setNewValue
								var newValue = $scope.nested.newValueBatchUpdate;
								if ($scope.nested.selectedVariableFilter.dataTypeCode === 'D') {
									newValue = $.datepicker.formatDate("yymmdd", newValue);
								}
								var param = JSON.stringify({
									newValue: newValue,
									newCategoricalValueId: getCategoricalValueId($scope.nested.newValueBatchUpdate, $scope.nested.selectedVariableFilter),
									observationUnitsSearchDTO: {
										instanceId: $scope.nested.selectedEnvironment.instanceId,
										draftMode: $scope.isPendingView,
										filter: getFilter()
									}
								});
								datasetService.setValueToVariable(subObservationSet.id, param).then(function () {
									if ($scope.isPendingView) {
										reloadDataset();
									} else {
										$scope.selectedStatusFilter = "1";
										$scope.nested.selectedEnvironment = $scope.environments[1];
										table().ajax.reload();
										loadBatchActionCombo();
									}
								}, function (response) {
									if (response.errors && response.errors.length) {
										showErrorMessage('', response.errors[0].message);
									} else {
										showErrorMessage('', ajaxGenericErrorMsg);
									}
								});
								break;
							case 2:
								// acceptDraftDataByVariable
								var param = JSON.stringify({
									instanceId: $scope.nested.selectedEnvironment.instanceId,
									draftMode: $scope.isPendingView,
									filter: getFilter()
								});
								datasetService.acceptDraftDataByVariable(subObservationSet.id, param).then(function () {
									reloadDataset();
								}, function (response) {
									if (response.errors && response.errors.length) {
										showErrorMessage('', response.errors[0].message);
									} else {
										showErrorMessage('', ajaxGenericErrorMsg);
									}
								});
								break;
							default:
								break;
						}
					});
				});
			};

			$scope.isVariableFilter = function (index) {
				if (!$scope.nested.selectedVariableFilter || !$scope.nested.selectedVariableFilter.termId) {
					return true
				}
				return $scope.isVariableBatchActionSelected(index);
			};

			$scope.isCheckBoxColumn = function (index) {
				return index === 0;
			};

			$scope.isVariableBatchActionSelected = function (index) {
				return $scope.nested.selectedVariableFilter //
					&& $scope.columnsObj //
					&& $scope.columnsObj.columns //
					&& $scope.columnsObj.columns[index] //
					&& $scope.columnsObj.columns[index].columnData //
					&& $scope.columnsObj.columns[index].columnData.termId === $scope.nested.selectedVariableFilter.termId;
			};

			$scope.filterByColumn = function () {
				resetChecksStatus();
				table().ajax.reload();
			};

			$scope.resetFilterByColumn = function () {
				$scope.columnFilter.columnData.query = '';
				$scope.columnFilter.columnData.sortingAsc = null;
				if ($scope.columnFilter.columnData.possibleValues) {
					$scope.columnFilter.columnData.possibleValues.forEach(function (value) {
						value.isSelectedInFilters = false;
					});
					$scope.columnFilter.columnData.isSelectAll = false;
				}
				resetChecksStatus();
				table().ajax.reload();
			};

			$scope.sortColumn = function (asc) {
				$scope.columnFilter.columnData.sortingAsc = asc;
				table().order([$scope.columnFilter.index, asc ? 'asc' : 'desc']).draw();
			};

			$scope.getFilteringByClass = function (index) {
				if (!$scope.columnsObj.columns[index]) {
					return;
				}
				var columnData = $scope.columnsObj.columns[index].columnData;
				if (columnData.isFiltered) {
					return 'filtering-by';
				}
			};


			function table() {
				return $scope.nested.dtInstance.DataTable;
			}

			function doPendingViewActions() {
				$scope.toggleSection = $scope.isPendingView;

				if ($scope.isPendingView) {
					$scope.nested.selectedEnvironment = $scope.environments[0];
				} else {
					$scope.nested.selectedEnvironment = $scope.environments[1];
				}
			}

			function getFilter() {
				var variableId = $scope.nested.selectedVariableFilter && $scope.nested.selectedVariableFilter.termId;
				return {
					byOutOfBound: $scope.selectedStatusFilter === "2" || null,
					byMissing: $scope.selectedStatusFilter === "3" || null,
					byOutOfSync: $scope.selectedStatusFilter === "4" || null,
					byOverwritten: $scope.selectedStatusFilter === "5" || null,
					variableId: variableId,
					filterColumns: [],
					filteredValues: $scope.columnsObj.columns.reduce(function (map, column) {
						var columnData = column.columnData;
						columnData.isFiltered = false;

						if (isTextFilter(columnData)) {
							return map;
						}

						if (columnData.possibleValues) {
							columnData.possibleValues.forEach(function (value) {
								if (value.isSelectedInFilters) {
									if (!map[columnData.termId]) {
										map[columnData.termId] = [];
									}
									map[columnData.termId].push(value.name);
								}
							});
							if (!map[columnData.termId] && columnData.query) {
								map[columnData.termId] = [columnData.query];
							}
						} else if (columnData.query) {
							if (columnData.dataType === 'Date') {
								map[columnData.termId] = [($.datepicker.formatDate("yymmdd", columnData.query))];
							} else {
								map[columnData.termId] = [(columnData.query)];
							}
						}

						if (map[columnData.termId]) {
							columnData.isFiltered = true;
						}
						return map;
					}, {}),
					filteredTextValues: $scope.columnsObj.columns.reduce(function (map, column) {
						var columnData = column.columnData;
						if (!isTextFilter(columnData)) {
							return map;
						}
						if (columnData.query) {
							map[columnData.termId] = columnData.query;
							columnData.isFiltered = true;
						}
						return map;
					}, {}),
					variableTypeMap: $scope.columnsObj.columns.reduce(function (map, column) {
						map[column.columnData.termId] = column.columnData.variableType;
						return map;
					}, {})
				};
			}

			function isTextFilter(columnData) {

				// Factors like GID, GROUPGID, LINE_GID and TESTER_GID have 'Germplasm List' datatype but they should be treated as numeric
				if (columnData.termId === GID || columnData.termId === GROUPGID
					|| columnData.termId === LINE_GID || columnData.termId === TESTER_GID) {
					return false;
				}

				if (columnData.dataType === 'Categorical' || columnData.dataType === 'Numeric'
					|| columnData.dataType === 'Date' || columnData.termId === SAMPLES) {
					return false;
				}

				return true;

			}

			function getDtOptions() {
				return addCommonOptions(DTOptionsBuilder.newOptions()
					.withOption('ajax',
						function (d, callback) {
							$.ajax({
								type: 'POST',
								url: datasetService.getObservationTableUrl(subObservationSet.id) + getPageQueryParameters(d),
								data: JSON.stringify({
									draw: d.draw,
									instanceId: $scope.nested.selectedEnvironment.instanceId,
									draftMode: $scope.isPendingView,
									filter: getFilter()
								}),
								success: function (res, status, xhr) {
									let json = {recordsTotal: 0, recordsFiltered: 0}
									json.recordsTotal = xhr.getResponseHeader('X-Total-Count');
									json.recordsFiltered = xhr.getResponseHeader('X-Filtered-Count');
									json.data = res;
									callback(json);
								},
								contentType: 'application/json',
								beforeSend: function (xhr) {
									xhr.setRequestHeader('X-Auth-Token', JSON.parse(localStorage['bms.xAuthToken']).token);
								},
							});
						})
					.withDataProp('data')
					.withOption('serverSide', true)
					.withOption('initComplete', initCompleteCallback)
					.withOption('headerCallback', headerCallback)
					.withOption('drawCallback', drawCallback));
			}

			function getPageQueryParameters(data) {
				var order = data.order && data.order[0];
				var pageQuery = '?size=' + data.length
					+ '&page=' + ((data.length === 0) ? 0 : data.start / data.length);
				if (subObservationSet.columnsData[order.column]) {
					pageQuery += '&sort=' + subObservationSet.columnsData[order.column].termId + ',' + order.dir;
				}
				return pageQuery;
			}

			function addCommonOptions(options) {
				return options
					.withOption('processing', true)
					.withOption('lengthMenu', [[50, 75, 100], [50, 75, 100]])
					.withOption('scrollY', '500px')
					.withOption('scrollCollapse', true)
					.withOption('scrollX', '100%')
					.withOption('language', {
						processing: '<span class="throbber throbber-2x"></span>',
						lengthMenu: 'Records per page: _MENU_',
						paginate: {
							next: '>',
							previous: '<',
							first: '<<',
							last: '>>'
						}
					})
					.withDOM('<"pull-left fbk-left-padding"r>' + //
						'<"pull-right"B>' + //
						'<"clearfix">' + //
						'<"row add-top-padding-small"<"col-sm-12"t>>' + //
						'<"row"<"col-sm-12 paginate-float-center"<"pull-left"i><"pull-right"l>p>>')
					.withButtons([{
						className: 'fbk-buttons-no-border fbk-colvis-button',
						text: '<img src="/Fieldbook/static/img/statistics.svg" height="16px" width="16px"></img>',
						action: function (e, dt, node, config) {

							var observationUnitsSearch = {
								// We send all the data (with/without filter) to OpenCPU server,
								// No pagination is needed.
								sortedRequest: null,
								instanceId: $scope.nested.selectedEnvironment.instanceId,
								draftMode: $scope.isPendingView,
								filter: getFilter()
							};

							visualizationModalService.openModal(subObservationSet.id, observationUnitsSearch);
						}
					}, {
						extend: 'colvis',
						className: 'fbk-buttons-no-border fbk-colvis-button',
						text: '<i class="glyphicon glyphicon-th"></i>',
						columns: ':gt(0)'
					}])
					.withColReorder()
					.withPaginationType('full_numbers');
			}

			function initCompleteCallback() {
				table().columns().every(function () {
					$(this.header())
						.prepend($compile('<span class="glyphicon glyphicon-bookmark" style="margin-right: 10px; color:#1b95b2;"' +
							' ng-if="isVariableBatchActionSelected(' + this.index() + ')"> </span>')($scope))
						.append($compile('<span ng-if="!isCheckBoxColumn(' + this.index() + ')" class="glyphicon glyphicon-filter" ' +
							' style="cursor:pointer; padding-left: 5px;"' +
							' popover-placement="bottom"' +
							' ng-class="getFilteringByClass(' + this.index() + ')"' +
							' popover-append-to-body="true"' +
							' popover-trigger="\'outsideClick\'"' +
							// does not work with outsideClick
							// ' popover-is-open="columnFilter.isOpen"' +
							' ng-if="isVariableFilter(' + this.index() + ')"' +
							' ng-click="openColumnFilter(' + this.index() + ')"' +
							' uib-popover-template="\'columnFilterPopoverTemplate.html\'"></span>')($scope))
						.prepend($compile('<span ng-if="isCheckBoxColumn(' + this.index() + ')">'
							+ '<input type="checkbox" title="select current page" ng-checked="isPageSelected()"  ng-click="onSelectPage()">'
							+ '</span>')($scope));
				});
				adjustColumns();
				tableRenderedResolve();
			}

			function headerCallback(thead, data, start, end, display) {
				table().columns().every(function () {
					var column = $scope.columnsObj.columns[this.index()];
					if (column.columnData.formula) {
						$(this.header()).addClass('derived-trait-column-header');
					}
				});
			}

			function drawCallback() {
				addCellClickHandler();
				adjustColumns();
			}

			function adjustColumns() {
				if ($scope.hasInstances) {
					$timeout(function () {
						table().columns.adjust();
					});
				}
			}

			/* WARNING Complexity up ahead.
			 * The following logic is probably one the most complex in the BMS system
			 * The previous version was even more complex, spanning several files and thousand of lines.
			 * A lot of effort was put to simplify that logic here.
			 * We hope we can make even simpler and clearer in the future
			 * TODO unit test
			 */
			function addCellClickHandler() {
				var $table = angular.element(tableId);

				addClickHandler();

				function addClickHandler() {
					$table.off('click').on('click', 'td.variates:not([disabled])', clickHandler);
				}

				function clickHandler() {
					var cell = this;

					var table = $table.DataTable();
					var dtRow = table.row(cell.parentNode);
					var rowData = dtRow.data();
					var dtCell = table.cell(cell);
					var cellData = dtCell.data();
					var index = table.colReorder.transpose(table.column(cell).index(), 'toOriginal');
					var columnData = $scope.columnsObj.columns[index].columnData;
					var termId = columnData.termId;

					if (!termId) return;

					/**
					 * Remove handler to not interfere with inline editor
					 * will be restored after fnUpdate
					 */
					$table.off('click');

					$scope.$apply(function () {

						var $inlineScope = $scope.$new(true);

						$inlineScope.observation = {
							value: $scope.isPendingView ? cellData.draftValue : cellData.value,
							change: function () {
								updateInline();
							},
							// FIXME altenative to blur bug https://github.com/angular-ui/ui-select/issues/499
							onOpenClose: function (isOpen) {
								if (!isOpen) updateInline();
							},
							newInlineValue: function (newValue) {
								return {name: newValue};
							}
						};

						$inlineScope.columnData = columnData;
						$inlineScope.isCategoricalDescriptionView = $scope.isCategoricalDescriptionView;

						$(cell).html('');
						var editor = $compile(
							' <observation-inline-editor ' +
							' is-categorical-description-view="isCategoricalDescriptionView" ' +
							' column-data="columnData" ' +
							' observation="observation"></observation-inline-editor> '
						)($inlineScope);

						$(cell).append(editor);

						function updateInline() {

							function doAjaxUpdate() {
								if ((!$scope.isPendingView && cellData.value === $inlineScope.observation.value)
									|| ($scope.isPendingView && cellData.draftValue === $inlineScope.observation.value)) {
									return $q.resolve(cellData);
								}

								var value = cellData.value;
								var draftValue = cellData.draftValue;

								if ($scope.isPendingView) {
									draftValue = $inlineScope.observation.value;
								} else {
									value = $inlineScope.observation.value;
								}

								if (cellData.observationId) {
									if (!value && !$scope.isPendingView) {
										if (cellData.draftValue) {
											value = null;
										} else {
											return datasetService.deleteObservation(subObservationSet.id, rowData.observationUnitId,
												cellData.observationId);
										}
									}

									return confirmOutOfBoundData(value, columnData).then(function (doContinue) {
										if (!doContinue) {
											$inlineScope.observation.value = cellData.value;
											return {observationId: cellData.observationId};
										}
										return datasetService.updateObservation(subObservationSet.id, rowData.observationUnitId,
											cellData.observationId, {
												categoricalValueId: getCategoricalValueId(value, columnData),
												value: value,
												draftValue: draftValue,
												draftMode: $scope.isPendingView
											});
									});
								}

								if (value) {
									return confirmOutOfBoundData(value, columnData).then(function (doContinue) {
										if (!doContinue) {
											$inlineScope.observation.value = cellData.value;
											return {observationId: cellData.observationId};
										}
										return datasetService.addObservation(subObservationSet.id, rowData.observationUnitId, {
											observationUnitId: rowData.observationUnitId,
											categoricalValueId: getCategoricalValueId(value, columnData),
											variableId: termId,
											value: value,
											draftMode: $scope.isPendingView
										});
									});
								}

								return $q.resolve(cellData);
							} // doAjaxUpdate

							var promise = doAjaxUpdate();

							promise.then(function (data) {
								var valueChanged = false;
								if (cellData.value !== $inlineScope.observation.value) {
									valueChanged = true;
								}

								if ($scope.isPendingView) {
									cellData.draftValue = $inlineScope.observation.value;
								} else {
									cellData.value = $inlineScope.observation.value;
								}

								cellData.observationId = data.observationId;
								cellData.status = data.status;

								$inlineScope.$destroy();
								editor.remove();

								/**
								 * We are updating the cell value and the target if the trait is input of a formula
								 * to avoid reloading the page. It has these advantages:
								 * - Make the inline edition more dynamic and fast
								 * - Don't reset the table scroll
								 *
								 * The alternative would be:
								 *
								 *     table.ajax.reload(function () {
								 *         // Restore handler
								 *         $table.off('click').on('click', 'td.variates', clickHandler);
								 *     }, false);
								 */
								dtCell.data(cellData);
								processCell(cell, cellData, rowData, columnData);

								if (valueChanged && $scope.columnDataByInputTermId[termId]) {
									angular.forEach($scope.columnDataByInputTermId[termId], function (targetColumnData) {
										var targetColIndex = table.colReorder.transpose(targetColumnData.index, 'toCurrent');
										var targetDtCell = table.cell(dtRow.node(), targetColIndex);
										var targetCellData = targetDtCell.data();
										targetCellData.status = 'OUT_OF_SYNC';
										processCell(targetDtCell.node(), targetCellData, rowData, targetColumnData);
									});
								}

								// Restore handler
								addClickHandler();
							}, function (response) {
								if (response.errors) {
									showErrorMessage('', response.errors[0].message);
								} else {
									showErrorMessage('', ajaxGenericErrorMsg);
								}
							});

						} // updateInline

						if (columnData.dataTypeCode === 'D') {
							$(cell).one('click', 'input', function () {
								var initialValue;
								try {
									initialValue = $.datepicker.formatDate("yy-mm-dd", $.datepicker.parseDate('yymmdd', $(this).val()));
								} catch (e) {
								}

								$(this).on('keydown', function (e) {
									if (e.keyCode === 13) {
										e.stopImmediatePropagation();
									}
								}).datepicker({
									format: 'yyyymmdd',
									todayHighlight: true,
									todayBtn: true,
									forceParse: false
								}).on('hide', function () {
									updateInline();
								}).datepicker("show").datepicker('update', initialValue)
							});
						}

						// FIXME show combobox for categorical traits
						$(cell).css('overflow', 'visible');

						$timeout(function () {
							/**
							 * Initiate interaction with the input so that clicks on other parts of the page
							 * will trigger blur immediately. Also necessary to initiate datepicker
							 * This also avoids temporary click handler on body
							 * FIXME is there a better way?
							 */
							$(cell).find('a.ui-select-match, input').click().focus();
						}, 100);
					});
				} // clickHandler
			}

			$scope.allTableItems = function () {
				return table().context[0].json && table().context[0].json['recordsFiltered'];
			};

			$scope.isPageSelected = function () {
				var currentItems = getCurrentItems();
				return $scope.selectedItems.length > 0 && !currentItems.some((item) => $scope.selectedItems.indexOf(item) === -1);
			};

			$scope.isSelected = function (itemId) {
				return itemId && $scope.selectedItems.length > 0 && $scope.selectedItems.find((item) => item === itemId);
			};

			$scope.toggleSelect = function (observationUnitId) {
				var idx = $scope.selectedItems.indexOf(observationUnitId);
				if (idx > -1) {
					$scope.selectedItems.splice(idx, 1)
				} else {
					$scope.selectedItems.push(observationUnitId);
				}
			};

			$scope.onSelectAllPages = function () {
				$scope.isAllPagesSelected = !$scope.isAllPagesSelected;
				table().columns(0).visible(!$scope.isAllPagesSelected);
				$scope.selectedItems = [];
				table().ajax.reload();
			};

			$scope.onSelectPage = function () {
				var currentItems = getCurrentItems();
				if ($scope.isPageSelected()) {
					$scope.selectedItems = $scope.selectedItems.filter((item) =>
						currentItems.indexOf(item) === -1);
				} else {
					$scope.selectedItems = currentItems.filter((item) =>
						$scope.selectedItems.indexOf(item) === -1
					).concat($scope.selectedItems);
				}
			};

			$scope.validateSelection = function () {
				if (!$scope.isAllPagesSelected && !$scope.selectedItems.length) {
					return showErrorMessage('', $.fieldbookMessages.errorPlantingSelectionInvalid);
				}
				return true;
			};

			function getCurrentItems() {
				return table().data().toArray().map((data) => {
					return data.observationUnitId
				});
			}

			function resetChecksStatus() {
				$scope.totalItems = 0;
				$scope.selectedItems = [];
				$scope.isAllPagesSelected = false;
				try {
					table().columns(0).visible(!$scope.isAllPagesSelected);
				} catch (e) {
				}
			}

			function getCategoricalValueId(cellDataValue, columnData) {
				if (columnData.possibleValues
					&& cellDataValue !== 'missing') {

					var categoricalValue = columnData.possibleValues.find(function (possibleValue) {
						return possibleValue.name === cellDataValue;
					});
					if (categoricalValue !== undefined) {
						return categoricalValue.id;
					}

				}
				return null;
			}

			function confirmOutOfBoundData(cellDataValue, columnData) {
				var deferred = $q.defer();

				if ($scope.isPendingView) {
					deferred.resolve(true);
					return deferred.promise;
				}

				var invalid = validateDataOutOfRange(cellDataValue, columnData);

				if (invalid) {
					var confirmModal = $scope.openConfirmModal(observationOutOfRange, keepLabel, discardLabel);
					confirmModal.result.then(deferred.resolve);
				} else {
					deferred.resolve(true);
				}

				return deferred.promise;
			}

			function reloadDataset() {
				$rootScope.navigateToSubObsTab(subObservationSet.id, {reload: true});
			}

			function loadTable() {
				/**
				 * We need to reinitilize all this because
				 * if we use column.visible an change the columns with just
				 *        $scope.dtColumns = columnsObj.columns;
				 * datatables is breaking with error:
				 * Cannot read property 'clientWidth' of null
				 */
				var dtColumnsPromise = $q.defer();
				var dtColumnDefsPromise = $q.defer();
				$scope.dtColumns = dtColumnsPromise.promise;
				$scope.dtColumnDefs = dtColumnDefsPromise.promise;
				$scope.dtOptions = null;

				return loadColumns().then(function (columnsObj) {
					$scope.dtOptions = getDtOptions();
					$scope.selectVariableFilter = [{
						alias: 'Please choose',
						name: 'Please choose',
						termId: null
					}];
					angular.forEach(columnsObj.columns, function (column, index) {
						// "PLOT_NO"
						if (column.columnData.termId === 8200) {
							$scope.dtOptions.withOption('order', [index, 'asc']);
						}
						if (!column.columnData.factor) {
							$scope.selectVariableFilter.push(column.columnData);
							$scope.hasVariableFilter = true;
						}
					});

					dtColumnsPromise.resolve(columnsObj.columns);
					dtColumnDefsPromise.resolve(columnsObj.columnsDef);

					tableLoadedResolve();
					loadBatchActionCombo();
					resetChecksStatus();
				});
			}

			function loadColumns() {
				return datasetService.getColumns(subObservationSet.id, $scope.isPendingView).then(function (columnsData) {
					subObservationSet.columnsData = addCheckBoxColumn(columnsData);
					var columnsObj = $scope.columnsObj = subObservationSet.columnsObj = mapColumns(subObservationSet.columnsData);
					return columnsObj;
				});
			}

			function addCheckBoxColumn(columnsData) {
				// copy array to avoid modifying the parameter (unit test might reuse the same object)
				var columns = columnsData.slice();
				columns.unshift({
					alias: "",
					factor: true,
					name: "CHECK",
					termId: -3,
				});
				return columns;
			}

			function mapColumns(columnsData) {
				$scope.columnDataByInputTermId = {};

				var columns = [],
					columnsDef = [];

				angular.forEach(columnsData, function (columnData, index) {
					if (columnData.possibleValues) {
						columnData.possibleValuesByName = {};
						columnData.possibleValuesById = {};
						angular.forEach(columnData.possibleValues, function (possibleValue) {
							// so we can use "Please Choose"=empty value
							possibleValue.displayValue = possibleValue.name;
							// convenience map to avoid looping later
							columnData.possibleValuesByName[possibleValue.name] = possibleValue;
							columnData.possibleValuesById[possibleValue.id] = possibleValue;
						});
						// waiting for https://github.com/angular-ui/ui-select/issues/152
						columnData.possibleValues.unshift({name: '', displayValue: 'Please Choose', displayDescription: 'Please Choose'});
					}

					// store formula info to update out-of-sync status after edit
					if (columnData.formula && columnData.formula.inputs) {
						columnData.formula.inputs.forEach(function (input) {
							if (!$scope.columnDataByInputTermId[input.id]) {
								$scope.columnDataByInputTermId[input.id] = [];
							}
							$scope.columnDataByInputTermId[input.id].push(columnData);
						});
					}
					columnData.index = index;

					function isColumnVisible() {

						if (columnData.termId === TRIAL_INSTANCE) {
							return $scope.nested.selectedEnvironment === $scope.environments[0]
						}
						return hiddenColumns.indexOf(columnData.termId) < 0;
					}

					function getClassName() {
						var className = columnData.factor === true ? 'factors' : 'variates';
						// include data type for determining when to show context menu option/s
						className += ' datatype-' + columnData.dataTypeId;
						// avoid wrapping filter icon
						className += ' dt-head-nowrap';
						return className;
					}

					columns.push({
						title: columnData.alias,
						name: columnData.alias,
						data: function (row) {
							return row.variables[columnData.name];
						},
						visible: isColumnVisible(),
						defaultContent: '',
						className: getClassName(),
						columnData: columnData
					});

					// CheckBox Column
					if (columnData.index === 0) {
						columnsDef.push({
							targets: columns.length - 1,
							orderable: false,
							createdCell: function (td, cellData, rowData, rowIndex, colIndex) {
								$(td).append($compile('<span><input type="checkbox" ng-checked="isSelected(' + rowData.observationUnitId + ')" ng-click="toggleSelect(' + rowData.observationUnitId + ')"></span>')($scope));
							}
						});
					} else if (columnData.termId === 8240 || columnData.termId === 8250) {
						// GID or DESIGNATION
						columnsDef.push({
							targets: columns.length - 1,
							orderable: false,
							render: function (data, type, full, meta) {
								return '<a class="gid-link" href="javascript: void(0)" ' +
									'onclick="openGermplasmDetailsPopopWithGidAndDesig(\'' +
									full.gid + '\',\'' + full.designation + '\')">' + EscapeHTML.escape(data.value) + '</a>';
							}
						});
					} else if (columnData.termId === STOCK_ID) {
						// STOCK_ID column
						columnsDef.push({
							targets: columns.length - 1,
							orderable: false,
							render: function (data, type, full, meta) {
								return full.stockId;
							}
						});
					} else if (columnData.termId === SAMPLES) {
						// SAMPLES count column
						columnsDef.push({
							targets: columns.length - 1,
							orderable: false,
							render: function (data, type, full, meta) {
								var programUUID = studyContext.programId;
								if (full.samplesCount !== '-') {
									return '<a class="gid-link" href="javascript: void(0)" ' +
										'onclick="openSampleSummary(\'' +
										full.variables['OBS_UNIT_ID'].value + '\', null, \'' + programUUID + '\')">' + EscapeHTML.escape(full.samplesCount) + '</a>';
								} else {
									return full.samplesCount;
								}
							}
						});
					} else if (!columnData.factor) { // variates
						columnsDef.push({
							targets: columns.length - 1,
							orderable: false,
							createdCell: function (td, cellData, rowData, rowIndex, colIndex) {
								processCell(td, cellData, rowData, columnData);
							},
							render: function (data, type, full, meta) {

								if (!data) {
									return '';
								}

								function renderByDataType(value, columnData) {
									if (columnData.dataTypeId === 1130) {
										return renderCategoricalValue(value, columnData);
									} else if (columnData.dataTypeId === 1110) {
										return getDisplayValueForNumericalValue(value);
									} else {
										return EscapeHTML.escape(value);
									}
								}

								var value = renderByDataType(data.value, columnData);
								if ($scope.isPendingView && data.draftValue !== null && data.draftValue !== undefined) {
									var existingValue = value;
									value = renderByDataType(data.draftValue, columnData);
									if (existingValue || existingValue === 0) {
										value += " (" + existingValue + ")";
									}
								}

								return value;
							}
						});
					} else {
						columnsDef.push({
							targets: columns.length - 1,
							orderable: false,
							render: function (data, type, full, meta) {

								if (!data) {
									return '';
								}

								if (columnData.dataTypeId === 1130) {
									return renderCategoricalValue(data.value, columnData);
								}

								return EscapeHTML.escape(data.value);
							}
						});
					}
				});

				return {
					columns: columns,
					columnsDef: columnsDef
				};
			}

			function renderCategoricalValue(value, columnData) {
				var possibleValue = null;

				if (columnData.possibleValues) {
					/* FIXME fix data model
					 *  Some variables don't store the cvterm.name (like traits in phenotype)
					 *  but the cvterm.cvterm_id (like treatment factors in nd_experimentprop).
					 *  This workaround will work most of the time with exception of out-of-bound categorical values that coincides
					 *  with the cvterm_id, though it's unlikely because the ids are not small numbers and it's not possible now to insert
					 *  outliers for categorical variables.
					 */
					possibleValue = columnData.possibleValuesByName[value] || columnData.possibleValuesById[value];
				}

				if (possibleValue
					&& possibleValue.displayDescription
					&& value !== 'missing') {

					var categoricalNameDom = '<span class="fbk-measurement-categorical-name"'
						+ ($scope.isCategoricalDescriptionView ? ' style="display: none; "' : '')
						+ ' >'
						+ EscapeHTML.escape(possibleValue.name) + '</span>';
					var categoricalDescDom = '<span class="fbk-measurement-categorical-desc"'
						+ (!$scope.isCategoricalDescriptionView ? ' style="display: none; "' : '')
						+ ' >'
						+ EscapeHTML.escape(possibleValue.displayDescription) + '</span>';

					value = categoricalNameDom + categoricalDescDom;
				}
				return value;
			}


			function getDisplayValueForNumericalValue(numericValue) {
				if (numericValue === "missing" || numericValue === "") {
					return numericValue;
				} else {
					return EscapeHTML.escape(numericValue ? Number(Math.round(numericValue + 'e4') + 'e-4') : '');
				}
			}

			function switchCategoricalView(showCategoricalDescriptionView) {
				'use strict';

				if (typeof showCategoricalDescriptionView === 'undefined') {
					showCategoricalDescriptionView = null;
				}

				return $.get('/Fieldbook/TrialManager/openTrial/setCategoricalDisplayType', {showCategoricalDescriptionView: showCategoricalDescriptionView})
					.done(function (result) {
						window.isCategoricalDescriptionView = result;

						$('.fbk-toggle-categorical-display').text(result ? window.measurementObservationMessages.hideCategoricalDescription :
							window.measurementObservationMessages.showCategoricalDescription);

					});
			}

			function validateNumericRange(minVal, maxVal, value, invalid) {
				if (parseFloat(value) < parseFloat(minVal) || parseFloat(value) > parseFloat(maxVal)) {

					invalid = true;
				}
				return invalid;
			}

			function validateCategoricalValues(columnData, cellDataValue, invalid) {
				if (columnData.possibleValues
					&& columnData.possibleValues.find(function (possibleValue) {
						return possibleValue.name === cellDataValue;
					}) === undefined
					&& cellDataValue !== 'missing') {

					invalid = true;
				}
				return invalid;
			}

			function validateDataOutOfRange(cellDataValue, columnData) {
				var invalid = false;

				var value = cellDataValue;
				var minVal = columnData.minRange;
				var maxVal = columnData.maxRange;

				invalid = validateNumericRange(minVal, maxVal, value, invalid);
				invalid = validateCategoricalValues(columnData, cellDataValue, invalid);
				return invalid;
			}

			function processCell(td, cellData, rowData, columnData) {
				var $td = $(td);
				$td.removeClass('accepted-value');
				$td.removeClass('invalid-value');
				$td.removeClass('manually-edited-value');

				if ($scope.isPendingView) {
					if (cellData.draftValue === null || cellData.draftValue === undefined) {
						$td.text('');
						$td.attr('disabled', true);
						return;
					}
					var invalid = validateDataOutOfRange(cellData.draftValue, columnData);

					if (invalid) {
						$td.addClass('invalid-value');
					}

					return;
				}

				if (cellData.value || cellData.value === 0) {
					var invalid = validateDataOutOfRange(cellData.value, columnData);

					if (invalid) {
						$td.addClass('accepted-value');
					}
				}
				if (cellData.status) {
					var status = cellData.status;
					if (!cellData.observationId) {
						return;
					}
					$td.removeAttr('title');
					$td.removeClass('manually-edited-value');
					$td.removeClass('out-of-sync-value');
					var toolTip = 'GID: ' + rowData.variables.GID.value + ' Designation: ' + rowData.variables.DESIGNATION.value;
					if (status === 'MANUALLY_EDITED') {
						$td.attr('title', toolTip + ' manually-edited-value');
						$td.addClass('manually-edited-value');
					} else if (status === 'OUT_OF_SYNC') {
						$td.attr('title', toolTip + ' out-of-sync-value');
						$td.addClass('out-of-sync-value');
					}
				}
			}

		}])
		.directive('observationInlineEditor', function () {
			return {
				restrict: 'E',
				templateUrl: '/Fieldbook/static/angular-templates/subObservations/observationInlineEditor.html',
				scope: {
					observation: '=',
					// TODO upgrade angular to > 1.5 to use one-way binding
					columnData: '=',
					isCategoricalDescriptionView: '='
				},
				controller: function ($scope) {
					$scope.doBlur = function ($event) {
						if ($event.keyCode === 13) {
							$event.target.blur();
						}
					}
				}
			};
		})
	;
})();

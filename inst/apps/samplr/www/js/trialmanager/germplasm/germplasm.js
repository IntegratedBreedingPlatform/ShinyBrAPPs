/*global angular, openListTree, displaySelectedGermplasmDetails*/

(function () {
	'use strict';

	var manageTrialAppModule = angular.module('manageTrialApp');

	manageTrialAppModule.controller('GermplasmCtrl',
		['$scope', '$rootScope', '$q', '$compile', 'TrialManagerDataService', 'DTOptionsBuilder', 'studyStateService', 'studyEntryService', 'germplasmStudySourceService',
			'datasetService', '$timeout', '$uibModal',
			function ($scope, $rootScope, $q, $compile, TrialManagerDataService, DTOptionsBuilder, studyStateService, studyEntryService, germplasmStudySourceService,
					  datasetService, $timeout, $uibModal) {

				$scope.settings = TrialManagerDataService.settings.germplasm;
				$scope.trialMeasurement = {hasMeasurement: studyStateService.hasGeneratedDesign()};
				$scope.isHideDelete = studyStateService.hasGeneratedDesign();
				$scope.addVariable = !studyStateService.hasGeneratedDesign();
				$scope.selectedItems = [];
				$scope.numberOfEntries = 0;

				var initResolve;
				$scope.initPromise = new Promise(function (resolve) {
					initResolve = resolve;
				});
				var dtColumnsPromise = $q.defer();
				var dtColumnDefsPromise = $q.defer();

				$scope.dtColumns = dtColumnsPromise.promise;
				$scope.dtColumnDefs = dtColumnDefsPromise.promise;
				$scope.dtOptions = null;

				loadTable();

				$rootScope.$on("reloadStudyEntryTableData", function(setShowValues){
					$scope.reloadStudyEntryTableData(setShowValues);
				});

				function table() {
					return $scope.nested.dtInstance.DataTable;
				}

				function getDtOptions() {
					return addCommonOptions(DTOptionsBuilder.newOptions()
						.withOption('ajax',
							function (d, callback) {
								$.ajax({
									type: 'POST',
									url: studyEntryService.getStudyEntriesUrl() + getPageQueryParameters(d),
									dataSrc: '',
									success: function (res, status, xhr) {
										let json = {recordsTotal: 0, recordsFiltered: 0};
										json.recordsTotal = json.recordsFiltered = xhr.getResponseHeader('X-Total-Count');
										json.data = res;
										setNumberOfEntries(json.recordsTotal);
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
					);
				}


				function getPageQueryParameters(data) {
					var order = data.order && data.order[0];
					var pageQuery = '?size=' + data.length
						+ '&page=' + ((data.length === 0) ? 0 : data.start / data.length);
					// FIXME: Until now the sort works with entryNumber when will implements by specific column we need replace the code by the commented.
					/*if ($scope.columnsData[order.column]) {
						pageQuery += '&sort=' + $scope.columnsData[order.column].termId + ',' + order.dir;
					}*/
					pageQuery += '&sort=8230' + ',' + order.dir;

					return pageQuery;
				}

				function setNumberOfEntries(numberOfEntries) {
					$scope.numberOfEntries = numberOfEntries;
					TrialManagerDataService.specialSettings.experimentalDesign.
						germplasmTotalListCount = $scope.numberOfEntries;
					$rootScope.$apply();
				}

				function addCommonOptions(options) {
					return options
						.withOption('processing', true)
						.withOption('lengthMenu', [[50, 75, 100], [50, 75, 100]])
						.withOption('scrollY', '500px')
						.withOption('scrollCollapse', true)
						.withOption('scrollX', '100%')
						.withOption('deferRender', true)
						.withOption('language', {
							processing: '<span class="throbber throbber-2x"></span>',
							lengthMenu: 'Records per page: _MENU_',
							paginate: {
								next: '>',
								previous: '<',
								first: '<<',
								last: '>>'
							}
						}).withDOM('<"row"<"col-sm-6"l>>' +
							'<"row"<"col-sm-12"tr>>' +
							'<"row"<"col-sm-5"i><"col-sm-7">>' +
							'<"row"<"col-sm-12"p>>')
						.withPaginationType('full_numbers');
				}

				function loadTable() {
					$scope.nested = {};
					$scope.nested.dtInstance = null;
					var dtColumnsPromise = $q.defer();
					var dtColumnDefsPromise = $q.defer();
					$scope.dtColumns = dtColumnsPromise.promise;
					$scope.dtColumnDefs = dtColumnDefsPromise.promise;
					$scope.dtOptions = null;

					return loadColumns().then(function (columnsObj) {
						$scope.selectedItems = [];
						$scope.dtOptions = getDtOptions();
						dtColumnsPromise.resolve(columnsObj.columns);
						dtColumnDefsPromise.resolve(columnsObj.columnsDef);
						initResolve();
					});
				}

				function loadColumns() {
					return studyEntryService.getEntryTableColumns().then(function (columnsData) {
						$scope.columnsData = addCheckBoxColumn(columnsData);
						var columnsObj = $scope.columnsObj = mapColumns($scope.columnsData);
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
						termId: -6,
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
						columnData.index = index;

						columns.push({
							title: columnData.alias,
							name: columnData.alias,
							data: function (row) {
								return row.properties[columnData.termId];
							},
							defaultContent: '',
							columnData: columnData
						});

						// CheckBox Column
						if (columnData.index === 0) {
							columnsDef.push({
								targets: columns.length - 1,
								orderable: false,
								createdCell: function (td, cellData, rowData, rowIndex, colIndex) {
									$(td).append($compile('<span><input type="checkbox" ng-click="toggleSelect(' + rowData.entryId + ')"></span>')($scope));
								}
							});
						} else if (columnData.termId === 8240 || columnData.termId === 8250) {
							// GID or DESIGNATION
							columnsDef.push({
								targets: columns.length - 1,
								orderable: false,
								createdCell: function (td, cellData, rowData, rowIndex, colIndex) {
									$(td).val("");
									var value = columnData.termId === 8240 ? rowData.properties['8240'].value : rowData.properties['8250'].value;
									$(td).append($compile('<a class="gid-link" href="javascript: void(0)" ' +
										'onclick="openGermplasmDetailsPopopWithGidAndDesig(\'' +
										rowData.gid + '\',\'' + rowData.designation + '\')">' + value + '</a>')($scope));
								},
								render: function (data, type, full, meta) {
									return '';
								}
							});
						} else if (columnData.termId === 8255) {
							// ENTRY_TYPE
							columnsDef.push({
								targets: columns.length - 1,
								orderable: false,
								createdCell: function (td, cellData, rowData, rowIndex, colIndex) {
									$(td).val("");
									$(td).append($compile('<a class="check-href edit-check' + rowData.entryId + '" href="javascript: void(0)" ' +
										'ng-click="showPopOverCheck(\'' + rowData.entryId + '\',\'' + rowData.properties['8255'].value + '\')">' +
										getCategoricalValue(rowData.properties['8255'].value, columnData) + '</a>')($scope));
								},
								render: function (data, type, full, meta) {
									return '';
								}
							});
						} else if (columnData.termId === -3) {
							//ACTIVE LOT
							columnsDef.push({
								targets: columns.length - 1,
								orderable: false,
								createdCell: function (td, cellData, rowData, rowIndex, colIndex) {
									if (rowData.lotCount === 0) {
										$(td).append('<span>-</span>');
									} else {
										$(td).html($compile('<a href ' +
											` ng-click="openInventoryDetailsModal('${rowData.gid}')"> ` +
											rowData.lotCount + '</a>')($rootScope));
										$rootScope.$apply();
									}
								}
							});

						} else if (columnData.termId === -4) {
							// AVAILABLE
							columnsDef.push({
								targets: columns.length - 1,
								orderable: false,
								render: function (data, type, full, meta) {
									return full.availableBalance;
								}
							});

						} else if (columnData.termId === -5) {
							// UNIT
							columnsDef.push({
								targets: columns.length - 1,
								orderable: false,
								render: function (data, type, full, meta) {
									return full.unit;
								}
							});
						}else {
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
						&& possibleValue.displayDescription) {

						value = '<span class="fbk-measurement-categorical-desc">'
							+ EscapeHTML.escape(possibleValue.description) + '</span>';
					}
					return value;
				}

				function getCategoricalValue(value, columnData) {
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

					if (possibleValue && possibleValue.displayDescription) {
						return possibleValue.description;
					}
					return value;
				}

				$scope.showImportListBrowser = !TrialManagerDataService.applicationData.germplasmListSelected;

				$scope.showStudyEntriesTable = TrialManagerDataService.applicationData.germplasmListSelected;

				$scope.showClearList = TrialManagerDataService.applicationData.germplasmListSelected && !studyStateService.hasGeneratedDesign();

				$scope.showUpdateImportListButton = TrialManagerDataService.applicationData.germplasmListSelected && !studyStateService.hasGeneratedDesign() && !$scope.showImportListBrowser;

				$scope.labels = {};
				$scope.labels.germplasmFactors = {
					label: 'Temp label here',
					placeholderLabel: 'Temp placeholder here'
				};

				$scope.updateOccurred = false;

				$scope.$on('deleteOccurred', function () {
					$scope.updateOccurred = true;
				});

				$scope.$on('variableAdded', function () {
					$scope.updateOccurred = true;
				});

				$scope.handleSaveEvent = function () {
					$scope.updateOccurred = false;
					TrialManagerDataService.specialSettings.experimentalDesign.germplasmTotalListCount = $scope.numberOfEntries;
				};

				// function called whenever the user has successfully selected a germplasm list
				$scope.germplasmListSelected = function () {
					// validation requiring user to re-generate experimental design after selecting new germplasm list is removed as per new maintain germplasm list functionality
					$scope.updateOccurred = false;
				};

				$scope.germplasmListCleared = function () {
					TrialManagerDataService.applicationData.germplasmListCleared = true;
					TrialManagerDataService.applicationData.germplasmListSelected = false;
				};

				$(document).on('germplasmListUpdated', function () {
					TrialManagerDataService.applicationData.germplasmListSelected = true;
				});

				$scope.openGermplasmTree = function () {
					openListTree(1, $scope.germplasmListSelected);
				};

				$scope.updateModifyList = function () {
					$scope.showImportListBrowser = true;
					$scope.showUpdateImportListButton = false;
					showGermplasmDetailsSection();
				};

				$scope.showUpdateImportList = function () {
					return $scope.showUpdateImportListButton;
				};

				$scope.showClearListButton = function() {
					return $scope.showClearList;
				}

				$scope.showImportList = function () {
					return $scope.showImportListBrowser;
				};

				$scope.showStudyTable = function() {
					return $scope.showStudyEntriesTable;
				}

				TrialManagerDataService.registerSaveListener('germplasmUpdate', $scope.handleSaveEvent);

				$scope.hasGeneratedDesign = function () {
					return studyStateService.hasGeneratedDesign();
				};

				$scope.disableAddButton = function () {
					return studyStateService.hasGeneratedDesign();
				};

				$scope.validateGermplasmForReplacement = function () {
					germplasmStudySourceService.searchGermplasmStudySources({}, 0, 1).then((germplasmStudySourceTable) => {

						// Check if study has advance or cross list
						if (germplasmStudySourceTable.length > 0) {
							showAlertMessage('', $.germplasmMessages.studyHasCrossesOrSelections);
						} else {
							if ($scope.selectedItems.length === 0) {
								showAlertMessage('', $.germplasmMessages.selectEntryForReplacement);
							} else if ($scope.selectedItems.length !== 1) {
								showAlertMessage('', $.germplasmMessages.selectOnlyOneEntryForReplacement);
							} else {
								$scope.replaceGermplasm($scope.selectedItems[0]);
							}
						}
					});
				};

				$scope.toggleSelect = function (data) {
					var idx = $scope.selectedItems.indexOf(data);
					if (idx > -1) {
						$scope.selectedItems.splice(idx, 1)
					} else {
						$scope.selectedItems.push(data);
					}
				};

				$scope.openReplaceGermplasmModal = function(entryId) {
					$rootScope.openGermplasmSelectorModal(false).then((gids) => {
						if (gids != null) {
							// if there are multiple entries selected, get only the first entry for replacement
							studyEntryService.replaceStudyGermplasm(entryId, gids[0]).then(function (response) {
								showSuccessfulMessage('', $.germplasmMessages.replaceGermplasmSuccessful);
								$rootScope.$emit("reloadStudyEntryTableData", {});
							}, function(errResponse) {
								showErrorMessage($.fieldbookMessages.errorServerError,  errResponse.errors[0].message);
							});
						}
					});
				};


				$scope.replaceGermplasm = function(entryId) {
					if (studyStateService.hasGeneratedDesign()) {
						var modalConfirmReplacement = $scope.openConfirmModal($.germplasmMessages.replaceGermplasmWarning, 'Yes','No');
						modalConfirmReplacement.result.then(function (shouldContinue) {
							if (shouldContinue) {
								$scope.openReplaceGermplasmModal(entryId);
							}
						});
					} else {
						$scope.openReplaceGermplasmModal(entryId);
					}

				};

				$scope.saveStudyEntries = function (listId) {

					studyEntryService.deleteEntries().then(function () {
						studyEntryService.saveStudyEntriesList(listId).then(function(res){
							TrialManagerDataService.applicationData.germplasmListSelected = true;
							$scope.reloadStudyEntryTableData();
							$scope.showImportListBrowser = false;
							$scope.showUpdateImportListButton = true;
							$scope.showStudyEntriesTable = true;
							$scope.showClearList = true;
						});
					});
				};

				$scope.resetStudyEntries = function() {
					var modalConfirmCancellation = $scope.openConfirmModal($.fieldbookMessages.confirmResetStudyEntries, 'Confirm', 'Cancel');
					modalConfirmCancellation.result.then(function (shouldContinue) {
						if (shouldContinue) {
							studyEntryService.deleteEntries().then(function () {
								$scope.numberOfEntries = 0;
								$scope.showImportListBrowser = true;
								$scope.showUpdateImportListButton = false;
								$scope.showStudyEntriesTable = false;
								$scope.showClearList = false;
								$scope.reloadStudyEntryTableData();
								TrialManagerDataService.applicationData.germplasmListSelected = false;
							});
						}
					});
				}

				$scope.onRemoveVariable = function (variableType, variableIds) {
					if(variableIds && variableIds.length !== 0) {
						var deferred = $q.defer();
						datasetService.getDatasets([4]).then(function (data) {
							angular.forEach(data, function (dataset) {
								datasetService.removeVariables(dataset.datasetId, variableIds).then(function () {
									deferred.resolve(true);
									showSuccessfulMessage('', $.germplasmMessages.removeVariableSuccess);
									$rootScope.navigateToTab('germplasm', {reload: true});
								}, function (response) {
									if (response.errors && response.errors.length) {
										showErrorMessage('', response.errors[0].message);
									} else {
										showErrorMessage('', ajaxGenericErrorMsg);
									}
								});
							});
						});
						return deferred.promise;
					} else {
						showAlertMessage('', $.germplasmMessages.removeVariableWarning);
					}
				};

				$scope.onAddVariable = function(result, variableType) {
					var variable = undefined;
					angular.forEach(result, function (val) {
						variable = val.variable;
					});
					datasetService.getDatasets([4]).then(function (data) {
						angular.forEach(data, function (dataset) {
							var variableName = variable.alias ? variable.alias : variable.name;
							datasetService.addVariables(dataset.datasetId, {
								variableTypeId: variableType,
								variableId: variable.cvTermId,
								studyAlias: variableName
							}).then(function () {
								showSuccessfulMessage('', $.germplasmMessages.addVariableSuccess.replace("{0}", variableName));
							})
						});
					});
				};

				$scope.onHideCallback = function () {
					$rootScope.navigateToTab('germplasm', {reload: true});
				};

				$scope.showPopOverCheck = function(entryIds, currentValue) {
					if(!studyStateService.hasGeneratedDesign()) {
						if(!Array.isArray(entryIds)) {
							entryIds = [parseInt(entryIds)];
						}
						$uibModal.open({
							templateUrl: '/Fieldbook/static/angular-templates/germplasm/changeStudyEntryTypeModal.html',
							controller: "changeStudyEntryTypeCtrl",
							size: 'md',
							resolve: {
								entryIds: function () {
									return entryIds;
								},
								currentValue: function () {
									return currentValue;
								}
							},
							controllerAs: 'ctrl'
						});
					}
				};

				$scope.setEntryTypeByBatch = function() {
					if ($scope.selectedItems.length === 0) {
						showAlertMessage('', $.germplasmMessages.setEntryTypeSelectEntry);
					} else {
						$scope.showPopOverCheck($scope.selectedItems, null);
					}
				};

				$scope.showAddEntriesModal = function() {
					$uibModal.open({
						templateUrl: '/Fieldbook/static/angular-templates/germplasm/addNewEntriesModal.html',
						controller: "AddNewEntriesController",
						size: 'md'
					});

				};

				$scope.reloadStudyEntryTableData = function(setShowValues) {
					$scope.selectedItems = [];
					table().ajax.reload();
					if(setShowValues) {
						$scope.showImportListBrowser = false;
						$scope.showStudyEntriesTable = true;
						$scope.showClearList = !studyStateService.hasGeneratedDesign();
						$scope.showUpdateImportListButton = !studyStateService.hasGeneratedDesign();
					}
				}
			}]);


})();

// README IMPORTANT: Code unmanaged by angular should go here

/* This will be called when germplasm details page is loaded */
(function() {
	'use strict';

	document.onLoadGermplasmDetails = function() {

		displayGermplasmListTreeTable('germplasmTree');

		changeBrowseGermplasmButtonBehavior(false);

		$('#listTreeModal').off('hide.bs.modal');
		$('#listTreeModal').on('hide.bs.modal', function() {
			TreePersist.saveGermplasmTreeState(true, '#germplasmTree');
			displayGermplasmListTreeTable('germplasmTree');
			changeBrowseGermplasmButtonBehavior(false);
			$(getDisplayedModalSelector() + ' #addGermplasmFolderDiv').hide();
			$(getDisplayedModalSelector() + ' #renameGermplasmFolderDiv').hide();
		});
	};

})();

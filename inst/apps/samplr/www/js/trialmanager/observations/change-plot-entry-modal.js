(function () {
	'use strict';

	const module = angular.module('manageTrialApp');

	var RADIO_BUTTON = -6,
	 	ENTRY_TYPE = 8255,
		GID = 8240,
		DESIGNATION = 8250,
		ENTRY_NO = 8230,
		CROSS = 8377,
		LOTS = -3,
		AVAILABLE = -4,
		UNITS = -5;

	module.controller('ChangePlotEntryModalCtrl', ['$scope', '$rootScope', 'studyContext', '$uibModalInstance', 'DTOptionsBuilder', 'DTColumnBuilder',
		'$timeout', '$q', '$compile', 'ChangePlotEntryService',
		function ($scope, $rootScope, studyContext, $uibModalInstance, DTOptionsBuilder, DTColumnBuilder, $timeout, $q, $compile, ChangePlotEntryService) {

			var initResolve;
			$scope.initPromise = new Promise(function (resolve) {
				initResolve = resolve;
			});

			$scope.selected = {entryId: ''};
			$scope.numberOfInstances = $scope.$resolve.numberOfInstances;
			$scope.numberOfPlots = $scope.$resolve.numberOfPlots;

			var dtColumnsPromise = $q.defer();
			var dtColumnDefsPromise = $q.defer();

			$scope.dtColumns = dtColumnsPromise.promise;
			$scope.dtColumnDefs = dtColumnDefsPromise.promise;
			$scope.dtOptions = null;

			$scope.nested = {};
			$scope.nested.dtInstance = null;

			loadTable();

			$scope.confirm = function () {
				proceed().then((doProceed) => {

					if (doProceed) {
						var observationUnitEntryReplaceRequest = {
							searchRequest: $scope.$resolve.searchComposite,
							entryId: $scope.selected.entryId
						}
						ChangePlotEntryService.updateObservationUnitsEntry(observationUnitEntryReplaceRequest, $scope.$resolve.datasetId).then(() => {
							$uibModalInstance.close();
							showSuccessfulMessage('', $.fieldbookMessages.changingPlotEntrySuccess);
						}, onError);
					}
				});
			};

			$scope.cancel = function () {
				$uibModalInstance.dismiss();
			};

			$scope.valid = function () {
				return $scope.selected.entryId != '';
			};

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

			$scope.filterByColumn = function () {
				resetRadioButtonStatus();
				table().ajax.reload();
			};

			$scope.isFilterable = function (index) {
				var columnData = $scope.columnsData[index];
				return columnData && columnData.termId !== RADIO_BUTTON && columnData.termId !== AVAILABLE;
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

			$scope.resetFilterByColumn = function () {
				$scope.columnFilter.columnData.query = '';
				$scope.columnFilter.columnData.sortingAsc = null;
				if ($scope.columnFilter.columnData.possibleValues) {
					$scope.columnFilter.columnData.possibleValues.forEach(function (value) {
						value.isSelectedInFilters = false;
					});
					$scope.columnFilter.columnData.isSelectAll = false;
				}
				resetRadioButtonStatus();
				table().ajax.reload();
			};

			function proceed() {
				var deferred = $q.defer();

				let confirmationMessages = $.fieldbookMessages.changingPlotEntryWarning;

				const message = '<ul>' + confirmationMessages + '</ul>';
				var confirmModal = $scope.openConfirmModal(message);
				confirmModal.result.then(deferred.resolve);
				return deferred.promise;
			}

			function onError(response) {
				if (response.errors && response.errors.length) {
					showErrorMessage('', response.errors[0].message);
				} else {
					showErrorMessage('', ajaxGenericErrorMsg);
				}
			}

			function loadTable() {
				var dtColumnsPromise = $q.defer();
				var dtColumnDefsPromise = $q.defer();
				$scope.dtColumns = dtColumnsPromise.promise;
				$scope.dtColumnDefs = dtColumnDefsPromise.promise;
				$scope.dtOptions = null;

				return loadColumns().then(function (columnsObj) {
					$scope.dtOptions = getDtOptions();
					$scope.dtOptions.withOption('order',
						[columnsObj.columns.findIndex(c => c.columnData.termId === ENTRY_NO), 'asc']);

					dtColumnsPromise.resolve(columnsObj.columns);
					dtColumnDefsPromise.resolve(columnsObj.columnsDef);
					initResolve();
				});
			}

			function loadColumns() {
				return ChangePlotEntryService.getColumns().then(function (columnsData) {
					$scope.columnsData = addRadioButtonColumn(columnsData);
					var columnsObj = $scope.columnsObj = mapColumns($scope.columnsData);
					return columnsObj;
				});
			}

			function addRadioButtonColumn(columnsData) {
				var columns = columnsData.slice();
				columns.unshift({
					alias: "",
					factor: true,
					name: "RADIO",
					termId: RADIO_BUTTON,
				});
				return columns;
			}

			function getDtOptions() {
				return addCommonOptions(DTOptionsBuilder.newOptions()
					.withOption('ajax',
						function (d, callback) {
							$.ajax({
								type: 'POST',
								url: ChangePlotEntryService.getEntriesTableUrl() + getPageQueryParameters(d),
								data: JSON.stringify({
									filter: getFilter()
								}),
								success: function (res, status, xhr) {
									let json = {recordsTotal: 0, recordsFiltered: 0}
									json.recordsTotal = json.recordsFiltered = xhr.getResponseHeader('X-Total-Count');
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
				);
			}

			function initCompleteCallback() {
				table().columns().every(function () {
					$(this.header())
						.append($compile('<span ng-if="isFilterable(' + this.index() + ')" class="glyphicon glyphicon-filter" ' +
							' style="cursor:pointer; padding-left: 5px;"' +
							' popover-placement="bottom"' +
							' ng-class="getFilteringByClass(' + this.index() + ')"' +
							' popover-append-to-body="true"' +
							' popover-trigger="\'outsideClick\'"' +
							' ng-click="openColumnFilter(' + this.index() + ')"' +
							' uib-popover-template="\'columnFilterPopoverTemplate.html\'"></span>')($scope));
				});
				adjustColumns();
			}

			function mapColumns(columnsData) {
				var columns = [],
					columnsDef = [];

				angular.forEach(columnsData, function (columnData, index) {
					if (columnData.possibleValues) {
						columnData.possibleValuesByName = {};
						columnData.possibleValuesById = {};
						angular.forEach(columnData.possibleValues, function (possibleValue) {
							possibleValue.displayValue = possibleValue.name;
							columnData.possibleValuesByName[possibleValue.name] = possibleValue;
							columnData.possibleValuesById[possibleValue.id] = possibleValue;
						});
					}

					columnData.index = index;

					columns.push({
						title: columnData.alias,
						name: columnData.alias,
						data: function (row) {
							return row.properties[columnData.termId];
						},
						defaultContent: '',
						className: 'factors dt-head-nowrap',
						columnData: columnData
					});

					// Radio Button Column
					if (columnData.termId === RADIO_BUTTON) {
						columnsDef.push({
							targets: columns.length - 1,
							orderable: false,
							createdCell: function (td, cellData, rowData, rowIndex, colIndex) {
								$(td).append($compile('<span><input type="radio" name="rowData.entryId" ng-model="selected.entryId" value=' + rowData.entryId + '></span>')($scope));
							}
						});
					} else if (columnData.termId === GID || columnData.termId === DESIGNATION) {
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
					} else if (columnData.termId === LOTS) {
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

					} else if (columnData.termId === AVAILABLE) {
						// AVAILABLE
						columnsDef.push({
							targets: columns.length - 1,
							orderable: false,
							render: function (data, type, full, meta) {
								return full.availableBalance;
							}
						});

					} else if (columnData.termId === UNITS) {
						// UNIT
						columnsDef.push({
							targets: columns.length - 1,
							orderable: false,
							render: function (data, type, full, meta) {
								return full.unit;
							}
						});
					}
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
				});

				return {
					columns: columns,
					columnsDef: columnsDef
				};
			}

			function getPageQueryParameters(data) {
				var order = data.order && data.order[0];
				var pageQuery = '?size=' + data.length
					+ '&page=' + ((data.length === 0) ? 0 : data.start / data.length);
				if ($scope.columnsData[order.column]) {
					pageQuery += '&sort=' + $scope.columnsData[order.column].termId + ',' + order.dir;
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

			function renderCategoricalValue(value, columnData) {
				var displayValue = '';

				if (columnData.possibleValues) {
					var possibleValue = columnData.possibleValuesByName[value] || columnData.possibleValuesById[value];
					displayValue = possibleValue.description;
				}
				return displayValue;

			}

			function table() {
				return $scope.nested.dtInstance.DataTable;
			}

			function adjustColumns() {
				$timeout(function () {
					table().columns.adjust();
				});
			}

			function getFilter() {
				return {
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
				if (columnData.termId === GID || columnData.termId === LOTS || columnData.dataType === 'Categorical'
					|| columnData.dataType === 'Numeric' || columnData.dataType === 'Date') {
					return false;
				}
				return true;

			}

			function resetRadioButtonStatus() {
				$scope.selected = {entryId: ''};
			}

		}
	]);
})();

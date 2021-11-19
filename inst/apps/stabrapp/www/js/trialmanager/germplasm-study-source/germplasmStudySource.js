(function () {
	'use strict';

	var germplasmStudySourceModule = angular.module('germplasm-study-source', []);

	germplasmStudySourceModule.controller('GermplasmStudySourceCtrl',
		['$rootScope', '$scope', '$q', '$compile', '$uibModal', 'studyContext', 'DTOptionsBuilder', 'germplasmStudySourceService', 'lotService',
			'HasAnyAuthorityService', 'PERMISSIONS',
			function ($rootScope, $scope, $q, $compile, $uibModal, studyContext, DTOptionsBuilder, germplasmStudySourceService, lotService,
					  HasAnyAuthorityService, PERMISSIONS) {

				$scope.hasAnyAuthority = HasAnyAuthorityService.hasAnyAuthority;
				$scope.PERMISSIONS = PERMISSIONS;

				$scope.nested = {};
				$scope.nested.dtInstance = null;

				const dtOptionsDeferred = $q.defer();
				$scope.dtOptions = dtOptionsDeferred.promise;

				const dtOptions = DTOptionsBuilder.newOptions()
					.withOption('ajax', function (d, callback) {
						$.ajax({
							type: 'POST',
							url: germplasmStudySourceService.getGermplasmStudySourceTableUrl() + getPageQueryParameters(d),
							data: JSON.stringify(addFilters({})),
							success: function (res, status, xhr) {
								let json = {recordsTotal: 0, recordsFiltered: 0}
								json.recordsFiltered = xhr.getResponseHeader('X-Filtered-Count');
								json.recordsTotal = xhr.getResponseHeader('X-Total-Count');
								json.data = res;
								setRecordsFiltered(json.recordsFiltered);
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
					.withOption('processing', true)
					.withOption('lengthMenu', [[50, 75, 100], [50, 75, 100]])
					.withOption('scrollY', '500px')
					.withOption('scrollCollapse', true)
					.withOption('scrollX', '100%')
					.withOption('order', [[2, 'desc']]) //gid
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
						extend: 'colvis',
						className: 'fbk-buttons-no-border fbk-colvis-button',
						text: '<i class="glyphicon glyphicon-th"></i>',
						columns: ':gt(0)'
					}])
					.withPaginationType('full_numbers');

				function getPageQueryParameters(data) {
					var order = data.order && data.order[0];
					return '?size=' + data.length
						+ '&page=' + ((data.length === 0) ? 0 : data.start / data.length)
						+ '&sort=' + $scope.dtColumns[order.column].name + ',' + order.dir;
				}

				function addFilters(request) {
					request.filter = {};
					Object.entries($scope.columns).forEach(([name, column]) => {
						if (column.filter && column.filter.transform) {
							column.filter.isFiltered = false;
							column.filter.transform(request);
						}
					});
					return request;
				}

				$scope.columns = {
					checkbox: {
						data: function () {
							return "";
						}
					},
					rowNumber: {
						data: function () {
							return "";
						}
					},
					gid: {
						data: 'gid',
						filter: {
							transform(request) {
								if (this.value) {
									request.filter.gidList = this.value.split(',');
									this.isFiltered = true;
								}
							}
						}
					},
					groupId: {
						data: function () {
							return '';
						},
						filter: {
							transform(request) {
								if (this.value) {
									request.filter.groupIdList = this.value.split(',');
									this.isFiltered = true;
								}
							}
						}
					},
					designation: {
						data: 'designation',
						filter: {
							transform(request) {
								if (this.value) {
									request.filter.designation = this.value;
									this.isFiltered = true;
								}
							}
						}
					},
					cross: {
						data: 'cross'
					},
					numberOfLots: {
						data: function () {
							return '';
						},
						filter: {
							transform(request) {
								if (this.value || this.value === 0) {
									request.filter.numberOfLotsList = this.value.split(',');
									this.isFiltered = true;
								}
							}
						}
					},
					breedingMethodAbbreviation: {
						data: 'breedingMethodAbbreviation',
						filter: {
							transform(request) {
								if (this.value) {
									request.filter.breedingMethodAbbreviation = this.value;
									this.isFiltered = true;
								}
							}
						}
					},
					breedingMethodName: {
						data: 'breedingMethodName',
						filter: {
							transform(request) {
								if (this.value) {
									request.filter.breedingMethodName = this.value;
									this.isFiltered = true;
								}
							}
						}
					},
					breedingMethodType: {
						data: 'breedingMethodType',
						filter: {
							transform(request) {
								if (this.value) {
									request.filter.breedingMethodType = this.value;
									this.isFiltered = true;
								}
							}
						}
					},
					breedingLocationName: {
						data: 'breedingLocationName',
						filter: {
							transform(request) {
								if (this.value) {
									request.filter.breedingLocationName = this.value;
									this.isFiltered = true;
								}
							}
						}
					},
					trialInstance: {
						data: 'trialInstance',
						filter: {
							transform(request) {
								if (this.value) {
									request.filter.trialInstanceList = this.value.split(',');
									this.isFiltered = true;
								}
							}
						}
					},
					plotNumber: {
						data: 'plotNumber',
						filter: {
							transform(request) {
								if (this.value) {
									request.filter.plotNumberList = this.value.split(',');
									this.isFiltered = true;
								}
							}
						}
					},
					replicationNumber: {
						data: 'replicationNumber',
						filter: {
							transform(request) {
								if (this.value) {
									request.filter.replicationNumberList = this.value.split(',');
									this.isFiltered = true;
								}
							}
						}
					},
					germplasmDate: {
						data: 'germplasmDate',
						filter: {
							transform(request) {
								if (this.value) {
									request.filter.germplasmDateList = this.value.split(',');
									this.isFiltered = true;
								}
							}
						}
					}
				};

				$scope.filterHelper = {
					filterByColumn(filter) {
						table().ajax.reload();
					},
					resetFilterByColumn(filter) {
						filter.value = null;
						if (filter.reset) {
							filter.reset();
						}
						table().ajax.reload();
					},
					sortColumn(columnName, asc) {
						table().order([$scope.dtColumns.findIndex((column) => column.name === columnName), asc ? 'asc' : 'desc']).draw();
					},
					isSortingAsc(columnName) {
						const index = $scope.dtColumns.findIndex((column) => column.name === columnName);
						const order = table().order().find((order) => order[0] === index);
						if (order) {
							return order[1] === 'asc';
						}
						return null;
					},
					getFilteringByClass(filter) {
						if (filter.isFiltered) {
							return 'filtering-by';
						}
					}
				};

				/**
				 * - column.name used for sorting
				 */
				$scope.dtColumns = Object.entries($scope.columns).map(([name, column]) => {
					return {
						name: name,
						data: column.data,
						visible: column.visible,
						orderable: column.orderable
					}
				});

				$scope.dtColumnDefs = [
					{
						// Checkbox
						targets: 0,
						createdCell: function (td, cellData, rowData, rowIndex, colIndex) {
							$(td).append($compile('<span><input type="checkbox" ng-checked="isSelected(' + rowData.gid + ')" ng-click="toggleSelect(' + rowData.gid + ')"></span>')($scope));
							$scope.$apply();
						}
					},
					{
						// Row Number
						targets: 1,
						render: function (data, type, rowData, meta) {
							return meta.row + 1 + table().page.info().start;
						}
					},
					{
						// GROUP ID
						targets: 3,
						createdCell: function (td, cellData, rowData, rowIndex, colIndex) {
							$(td).append('<span>' + (rowData.groupId != 0 ? rowData.groupId : '-') + '</span>');
						}
					},
					{
						// LOTS
						targets: 6,
						createdCell: function (td, cellData, rowData, rowIndex, colIndex) {
							if (rowData.numberOfLots === 0) {
								$(td).append('<span>-</span>');
							} else {
								$(td).html($compile('<a href ' +
									` ng-click="openInventoryDetailsModal('${rowData.gid}')"> ` +
									rowData.numberOfLots + '</a>')($rootScope));
								$rootScope.$apply();
							}
						}
					},
					{
						targets: "germplasm-link-column",
						render: function (data, type, rowData, meta) {
							return '<a class="gid-link" href="javascript: void(0)"'
								+ ` onclick="openGermplasmDetailsPopopWithGidAndDesig('${rowData.gid}','${rowData.designation}')">`
								+ EscapeHTML.escape(data) + '</a>';
						}
					},
					{
						targets: "_all",
						orderable: false
					}

				];

				$scope.totalItems = 0;
				$scope.selectedItems = {};
				$scope.isAllPagesSelected = false;

				$scope.isPageSelected = function () {
					var pageItemIds = getPageItemIds();
					return $scope.size($scope.selectedItems) > 0 && pageItemIds.every((item) => $scope.selectedItems[item]);
				};

				$scope.onSelectPage = function () {
					var pageItemIds = getPageItemIds();
					if ($scope.isPageSelected()) {
						// remove all items
						pageItemIds.forEach((item) => delete $scope.selectedItems[item]);
					} else {
						// check remaining items
						pageItemIds.forEach((item) => $scope.selectedItems[item] = true);
					}
				};

				$scope.onSelectAllPages = function () {
					$scope.isAllPagesSelected = !$scope.isAllPagesSelected;
					table().columns(0).visible(!$scope.isAllPagesSelected);
					$scope.selectedItems = {};
				};

				$scope.getRecordsFiltered = function () {
					// Replacing this as table().context doesn't contain json property
					return $scope.recordsFiltered && $scope.recordsFiltered;
				};

				$scope.isSelected = function (itemId) {
					return $scope.selectedItems[itemId];
				};

				$scope.toggleSelect = function (itemId) {
					if ($scope.selectedItems[itemId]) {
						delete $scope.selectedItems[itemId];
					} else {
						$scope.selectedItems[itemId] = true;
					}
				};

				$scope.size = function (obj) {
					return Object.keys(obj).length;
				};

				$scope.openLotCreationModal = function () {
					if ($scope.size($scope.selectedItems) || $scope.isAllPagesSelected) {
						lotService.saveSearchRequest({gids: Object.keys($scope.selectedItems)}).then((searchDto) => {
								$uibModal.open({
									templateUrl: '/Fieldbook/static/js/trialmanager/inventory/lot-creation/lot-creation-modal.html',
									controller: 'LotCreationCtrl',
									windowClass: 'app-modal-window',
									resolve: {
										searchResultDbId: function () {
											return searchDto.result.searchResultDbId;
										}
									}
								}).result.finally(function () {
									// Refresh and show the 'Crosses and Selections' tab
									$rootScope.navigateToTab('germplasmStudySource', {reload: true});
									$rootScope.$broadcast('inventoryChanged');
								});
							});
					} else {
						showErrorMessage('', $.fieldbookMessages.crossesAndSelectionsNoGermplasmError);
					}

				}

				function getPageItemIds() {
					const dataTable = table();
					if (!dataTable) {
						return [];
					}
					return dataTable.data().toArray().map((data) => {
						return data.gid;
					});
				}

				function table() {
					return $scope.nested.dtInstance.DataTable;
				}

				function setRecordsFiltered(recordsFiltered) {
					$scope.recordsFiltered = recordsFiltered;
				}

				dtOptionsDeferred.resolve(dtOptions);


			}]);

})();

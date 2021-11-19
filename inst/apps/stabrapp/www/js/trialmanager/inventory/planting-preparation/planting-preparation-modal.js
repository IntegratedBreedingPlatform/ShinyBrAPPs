(function () {
	'use strict';

	const module = angular.module('manageTrialApp');

	module.controller('PlantingPreparationModalCtrl', ['$scope', '$rootScope', 'studyContext', '$uibModalInstance', 'DTOptionsBuilder', 'DTColumnBuilder',
		'PlantingPreparationService', 'InventoryService', '$timeout', '$q', 'HasAnyAuthorityService', 'PERMISSIONS',
		function ($scope, $rootScope, studyContext, $uibModalInstance, DTOptionsBuilder, DTColumnBuilder, service, InventoryService, $timeout, $q,
				  HasAnyAuthorityService, PERMISSIONS) {

			$scope.hasAnyAuthority = HasAnyAuthorityService.hasAnyAuthority;
			$scope.PERMISSIONS = PERMISSIONS;

			// used also in tests - to call $rootScope.$apply()
			var initResolve;
			$scope.initPromise = new Promise(function (resolve) {
				initResolve = resolve;
			});

			$scope.unitsDTOptions = DTOptionsBuilder.newOptions().withDOM('<"row"<"col-sm-12"tr>>');
			$scope.entriesDTOptions = DTOptionsBuilder.newOptions().withDOM('<"row"<"col-sm-6"l><"col-sm-6"f>>' +
				'<"row"<"col-sm-12"tr>>' +
				'<"row"<"col-sm-5"i><"col-sm-7">>' +
				'<"row"<"col-sm-12"p>>')
				.withOption('processing', true)
				.withOption('lengthMenu', [[20, 50, 75, 100], [20, 50, 75, 100]])
				.withOption('scrollY', '300px')
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
				})
				.withPaginationType('full_numbers');
			$scope.nested = {}
			$scope.nested.entriesDTInstance = null;

			$scope.isCommitOnSaving = false;
			$scope.notes = "";
			$scope.numberOfInstances = 0;
			$scope.numberOfPlots = 0;

			/** { "unitId": { "entryNo1": entryObj } } */
			$scope.entryMap = {};

			service.getPlantingPreparationData($scope.$resolve.searchComposite, $scope.$resolve.datasetId).then(function (data) {
				return $scope.transformData(data);
			}, onError);

			$scope.transformData = function (data) {
				return InventoryService.queryUnits().then((unitTypes) => {
					const unitsById = unitTypes.reduce((unitsById, unitType) => {
						unitsById[unitType.id] = unitType.name
						return unitsById;
					}, {})

					$scope.entries = data.entries;

					$scope.entries.forEach((entry) => {
						if ($scope.size(entry.stockByStockId)) {
							// not possible to compare units, order by availableBalance at least
							entry.stockSelected = Object.entries(entry.stockByStockId)
								.sort((a, b) => b[1].availableBalance - a[1].availableBalance)[0][1];

							for (const stock of Object.values(entry.stockByStockId)) {
								if (!$scope.entryMap[stock.unitId]) {
									$scope.entryMap[stock.unitId] = {};
								}
								$scope.entryMap[stock.unitId][entry.entryNo] = entry;
							}
						}
					});
					$scope.units = $scope.entries
						.reduce((unitIds, entry) => {
							return unitIds.concat(Object.values(entry.stockByStockId).map((stock) => stock.unitId));
						}, [])
						.reduce((units, unitId) => {
							units[unitId] = {unitName: unitsById[unitId], groupTransactions: true};
							return units;
						}, {});

					$scope.numberOfInstances = Object.keys($scope.entries.reduce((instanceMap, entry) => {
						entry.numberOfPackets = entry.observationUnits.length;
						$scope.numberOfPlots += entry.observationUnits.length;
						entry.observationUnits.forEach((ou) => instanceMap[ou.instanceId] = true);
						return instanceMap;
					}, {})).length;

					if ($scope.size($scope.units) === 0) {
						showErrorMessage('', $.fieldbookMessages.plantingNoStockError);
					}

					initResolve();
				}, onError);
			};

			$scope.onGroupTransactionsChecked = function (unitId, unit) {
				if (!unit.groupTransactions) {
					unit.withdrawAll = false;
				}
				$scope.revalidateEntries(unitId);
			};

			$scope.onWithdrawAllChecked = function (unitId, unit) {
				if (unit.withdrawAll) {
					unit.amountPerPacket = null;
				}
				$scope.revalidateEntries(unitId);
			};

			$scope.onAmountPerPacketChanged = function (unitId, unit) {
				$scope.revalidateEntries(unitId);
			};

			$scope.revalidateEntries = function (unitId) {
				const entryMap = $scope.entryMap[unitId];
				if (entryMap) {
					for (const [entryNo, entry] of Object.entries(entryMap)) {
						entryMap[Number(entryNo)].valid = $scope.isValid(entry);
					}
				}
				adjustColumns();
			};

			$scope.stockIdChange = function (entry) {
				const entryMap = $scope.entryMap[entry.stockSelected.unitId];
				if (!entryMap) {
					return;
				}
				entryMap[entry.entryNo].valid = $scope.isValid(entry);
				adjustColumns();
			};

			$scope.isValid = function (entry) {
				if (!($scope.units && entry)) {
					return false;
				}
				const unit = $scope.units[entry.stockSelected.unitId];
				const stock = entry.stockByStockId[entry.stockSelected.stockId];
				if (!(unit && stock)) {
					return false;
				}
				if (unit.withdrawAll) {
					return stock.availableBalance > 0;
				}
				return unit.amountPerPacket > 0 && //
					unit.amountPerPacket * entry.numberOfPackets <= stock.availableBalance;
			};

			$scope.validLotsCount = function (unitId) {
				const entryMap = $scope.entryMap[unitId];
				if (!entryMap) {
					return;
				}
				return Object.values(Object.values(entryMap))
					.filter((entry) => entry.valid && entry.stockSelected.unitId === Number(unitId)).length
			};

			$scope.confirm = function () {
				validate().then((doProceed) => {
					if (doProceed) {
						service.confirmPlanting(getPlantingRequest(), $scope.$resolve.datasetId, $scope.isCommitOnSaving).then(() => {
							$uibModalInstance.close();
							showSuccessfulMessage('', $.fieldbookMessages.plantingSuccess);
							$rootScope.$broadcast('inventoryChanged')
						}, onError);
					}
				});
			};

			$scope.size = function (obj) {
				return Object.keys(obj).length;
			};

			$scope.keys = function (obj) {
				return Object.keys(obj);
			};

			$scope.cancel = function () {
				$uibModalInstance.dismiss();
			};

			$scope.valid = function () {
				return $scope.entries && $scope.entries.some((entry) => entry.valid);
			};

			function validate() {
				var deferred = $q.defer();

				let confirmationMessages = $scope.entries.some((entry) => !entry.valid) ? [$.fieldbookMessages.plantingNokWarning] : [];

				const metadataPromise = service.getMetadata(getPlantingRequest(), $scope.$resolve.datasetId);

				metadataPromise.then((metadata) => {
					if (metadata.pendingTransactionsCount) {
						confirmationMessages.push($.fieldbookMessages.plantingPendingTransactionsWarning);
					}
					if (metadata.confirmedTransactionsCount) {
						confirmationMessages.push($.fieldbookMessages.plantingConfirmedTransactionsWarning);
					}

					if (confirmationMessages.length) {
						const message = '<ul>' + confirmationMessages.map((m) => `<li>${m}</li>`).join('')
							+ '</ul><p>Do you want to proceed?</p>';
						var confirmModal = $scope.openConfirmModal(message);
						confirmModal.result.then(deferred.resolve);
					} else {
						deferred.resolve(true);
					}
				}, onError);

				return deferred.promise;
			}

			function getPlantingRequest() {
				const validEntries = $scope.entries.filter((entry) => entry.valid);
				const withdrawalsPerUnit = Object.entries($scope.units)
					.filter(([unitId, unit]) => Object.values($scope.entryMap[unitId]).some((entry) => {
						return entry.valid && entry.stockSelected.unitId === Number(unitId)
					}))
					.reduce((withdrawalsPerUnit, [unitId, unit]) => {
						withdrawalsPerUnit[unit.unitName] = {
							groupTransactions: unit.groupTransactions,
							withdrawAllAvailableBalance: unit.withdrawAll,
							withdrawalAmount: unit.amountPerPacket
						};
						return withdrawalsPerUnit;
					}, {});
				return {
					selectedObservationUnits: $scope.$resolve.searchComposite,
					withdrawalsPerUnit: withdrawalsPerUnit,
					lotPerEntryNo: validEntries.map((entry) => {
						return {
							entryNo: entry.entryNo,
							lotId: entry.stockSelected.lotId
						}
					}),
					notes: $scope.notes
				};
			}

			function onError(response) {
				if (response.errors && response.errors.length) {
					showErrorMessage('', response.errors[0].message);
				} else {
					showErrorMessage('', ajaxGenericErrorMsg);
				}
			}

			function adjustColumns() {
				$timeout(function () {
					$scope.nested.entriesDTInstance.DataTable.columns.adjust();
				});
			}
		}
	]);
})();

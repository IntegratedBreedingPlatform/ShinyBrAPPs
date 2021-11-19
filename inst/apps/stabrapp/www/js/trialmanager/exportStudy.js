(function () {
	'use strict';


	var exportStudyModule = angular.module('export-study', ['ui.bootstrap', 'datasets-api', 'datasetOptionModal', 'fieldbook-utils']);

	exportStudyModule.factory('exportStudyModalService', ['$uibModal',
		function ($uibModal) {

			var exportStudyModalService = {};

			exportStudyModalService.openDatasetOptionModal = function () {
				$uibModal.open({
					template: '<dataset-option-modal modal-title="modalTitle" message="message"' +
					'selected="selected" on-continue="showExportOptions()"></dataset-option-modal>',
					controller: 'exportDatasetOptionCtrl',
					size: 'md'
				});
			};

			exportStudyModalService.openExportStudyModal = function (datasetId) {
				$uibModal.open({
					templateUrl: '/Fieldbook/static/angular-templates/exportStudy/exportStudyModal.html',
					controller: "exportStudyCtrl",
					size: 'md',
					resolve: {
						datasetId: function () {
							return datasetId;
						}
					},
					controllerAs: 'ctrl'
				});
			};

			exportStudyModalService.showAlertMessage = function (title, message) {
				// Call the global function to show alert message
				showAlertMessage(title, message);
			};

			return exportStudyModalService;

		}]);

	exportStudyModule.controller('exportDatasetOptionCtrl', ['$scope', '$uibModal', '$uibModalInstance', 'studyContext', 'exportStudyModalService',
		function ($scope, $uibModal, $uibModalInstance, studyContext, exportStudyModalService) {

			$scope.modalTitle = 'Export study book';
			$scope.message = 'Please choose the dataset you would like to export:';
			$scope.measurementDatasetId = studyContext.measurementDatasetId;
			$scope.selected = {datasetId: $scope.measurementDatasetId};

			$scope.showExportOptions = function () {
				exportStudyModalService.openExportStudyModal($scope.selected.datasetId);
			};

		}]);

	exportStudyModule.controller('exportStudyCtrl', ['datasetId', '$scope', '$rootScope', '$uibModalInstance', 'datasetService', 'exportStudyModalService',
		'TrialManagerDataService', 'fileDownloadHelper',
		function (datasetId, $scope, $rootScope, $uibModalInstance, datasetService, exportStudyModalService, TrialManagerDataService, fileDownloadHelper) {


			var PLOT_ORDER = '1';
			var SERPENTINE_ALONG_ROWS_ORDER = '2';
			var SERPENTINE_ALONG_COLUMNS_ORDER = '3';

			var ctrl = this;

			ctrl.selectedExportFormatId = 'xls';
			ctrl.selectedCollectionOrderId = '1';
			ctrl.singleFile = true;

			$scope.exportFormats = [
				{key: 'csv', name: 'CSV'},
				{key: 'xls', name: 'Excel'},
				{key: 'ksu_csv', name: 'KSU Fieldbook CSV'},
				{key: 'ksu_xls', name: 'KSU Fieldbook Excel'}
			];

			$scope.collectionOrders = [
				{itemId: PLOT_ORDER, name: 'Plot Order'},
				{itemId: SERPENTINE_ALONG_ROWS_ORDER, name: 'Serpentine - Along Rows'},
				{itemId: SERPENTINE_ALONG_COLUMNS_ORDER, name: 'Serpentine - Along columns'}
			];

			$scope.instances = [];
			$scope.selectedInstances = {};
			$scope.isEmptySelection = false;

			$scope.cancel = function () {
				$uibModalInstance.dismiss();
			};

			$scope.proceed = function () {

				var instanceIds = ctrl.getSelectedInstanceIds();

				if (ctrl.selectedCollectionOrderId !== PLOT_ORDER) {
					ctrl.checkIfInstancesHaveFieldMap(instanceIds);
				} else {
					ctrl.export(instanceIds);
				}

			};

			ctrl.getSelectedInstanceIds = function () {

				var instanceIds = [];

				Object.keys($scope.selectedInstances).forEach(function (instanceId) {
					var isSelected = $scope.selectedInstances[instanceId];
					if (isSelected) {
						instanceIds.push(instanceId);
					}
				});

				return instanceIds;

			};


			ctrl.checkIfInstancesHaveFieldMap = function (instanceIds) {

				// If some of the selected instances don't have fieldmap, show the confirm popup.
				var someInstancesHaveNoFieldmap = $scope.instances.some(
					function (item) {
						return !item.hasFieldmap && instanceIds.indexOf(item.instanceId.toString()) > -1;
					});

				if (someInstancesHaveNoFieldmap) {
					ctrl.showConfirmModal(instanceIds);
				} else {
					ctrl.export(instanceIds);
				}

			};

			ctrl.showConfirmModal = function (instanceIds) {
				// Existing Trial with measurement data
				var modalInstance = $rootScope.openConfirmModal('Some of the environments you selected do not have field plans and so must ' +
					'be exported in plot order. Do you want to proceed?', 'Proceed');
				modalInstance.result.then(function (shouldContinue) {
					if (shouldContinue) {
						ctrl.export(instanceIds);
					}
				});
			};

			ctrl.export = function (instanceIds) {
				datasetService.exportDataset(datasetId, instanceIds, ctrl.selectedCollectionOrderId, ctrl.singleFile, ctrl.selectedExportFormatId).then(function (response) {
					var fileName = fileDownloadHelper.getFileNameFromResponseContentDisposition(response);
					fileDownloadHelper.save(response.data, fileName);
					$uibModalInstance.close();
				});

			};

			ctrl.init = function () {
				datasetService.getDatasetInstances(datasetId).then(function (instances) {
					$scope.instances = instances;
				});
			};

			ctrl.init();

		}]);

})();

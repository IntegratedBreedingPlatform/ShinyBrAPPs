(function () {
	'use strict';

	var importObservationAfterMappingVariateGroupDeRegister = function () {};

	var importStudyModule = angular.module('import-study', ['ui.bootstrap', 'datasets-api', 'datasetOptionModal',
		'fieldbook-utils']);

	importStudyModule.factory('importStudyModalService', ['$uibModal',
		function ($uibModal) {

			var importStudyModalService = {};

			importStudyModalService.openDatasetOptionModal = function () {
				$uibModal.open({
					template: '<dataset-option-modal modal-title="modalTitle" message="message"' +
						'selected="selected" on-continue="showImportOptions()"></dataset-option-modal>',
					controller: 'importDatasetOptionCtrl',
					size: 'md'
				});
			};

			importStudyModalService.openImportStudyModal = function (datasetId) {
				$uibModal.open({
					templateUrl: '/Fieldbook/static/angular-templates/importStudy/ImportStudyModal.html',
					controller: "importStudyCtrl",
					size: 'md',
					resolve: {
						datasetId: function () {
							return datasetId;
						}
					},
					controllerAs: 'ctrl'
				});
			};

			importStudyModalService.redirectToOldImportModal = function () {
				// Call the global function to show the old import study modal
				setTimeout(function () {
					showImportOptions();
				});
			};

			importStudyModalService.showAlertMessage = function (title, message) {
				// Call the global function to show alert message
				showAlertMessage(title, message);
			};

			importStudyModalService.showWarningMessage = function (header, title, warnings, question, confirmButtonLabel, cancelButtonLabel) {
				var modalInstance = $uibModal.open({
					animation: true,
					templateUrl: '/Fieldbook/static/angular-templates/warningModal.html',
					controller: function ($scope, $uibModalInstance) {
						$scope.header = header;
						$scope.title = title;
						$scope.warnings = warnings;
						$scope.question = question;
						$scope.confirmButtonLabel = confirmButtonLabel;
						$scope.cancelButtonLabel = cancelButtonLabel;

						$scope.confirm = function () {
							$uibModalInstance.close(true);
						};

						$scope.cancel = function () {
							$uibModalInstance.close(false);
						};
					}
				});
				return modalInstance;
			};

			return importStudyModalService;

		}]);

	importStudyModule.controller('importDatasetOptionCtrl', ['$scope', '$uibModal', '$uibModalInstance', 'studyContext', 'importStudyModalService',
		function ($scope, $uibModal, $uibModalInstance, studyContext, importStudyModalService) {

			$scope.modalTitle = 'Import observations';
			$scope.message = 'Please choose the dataset you would like to import:';
			$scope.measurementDatasetId = studyContext.measurementDatasetId;
			$scope.selected = {datasetId: $scope.measurementDatasetId};

			$scope.showImportOptions = function () {
				importStudyModalService.openImportStudyModal($scope.selected.datasetId);
			};

		}]);

	importStudyModule.controller('importStudyCtrl', ['datasetId', '$scope', '$rootScope', '$uibModalInstance', 'datasetService', 'importStudyModalService',
		'TrialManagerDataService',
		function (datasetId, $scope, $rootScope, $uibModalInstance, datasetService, importStudyModalService) {

			$scope.modalTitle = 'Import observations';
			$scope.file = null;
			$scope.importedData = null;
			var ctrl = this;

			// Deregister previously create listeners
			importObservationAfterMappingVariateGroupDeRegister();

			importObservationAfterMappingVariateGroupDeRegister = $rootScope.$on('importObservationAfterMappingVariateGroup', function (event) {
				$scope.importObservations(true);
			});

			ctrl.importFormats = [
				{name: 'CSV', extension: '.csv'}, //
				{name: 'Excel', extension: '.xls,.xlsx'}, //
				{name: 'KSU fieldbook CSV', extension: '.csv'}, //
				{name: 'KSU fieldbook Excel', extension: '.xls,.xlsx'} //
			];

			$scope.backToDatasetOptionModal = function () {
				$uibModalInstance.close();
				importStudyModalService.openDatasetOptionModal();
			};

			$scope.clearSelectedFile = function () {
				$scope.file = null;
				$scope.importedData = null;
			};

			$scope.submitImport = function () {
				$scope.validateNewVariables().then(function (result) {
					$rootScope.importedData  = $scope.importedData;
					if (result.length > 0) {
						ctrl.showAddVariableConfirmModal(result, datasetId);
					} else {
						$scope.importObservations(true);
					}
				});
			};

			$scope.validateNewVariables = function () {
				return datasetService.getAllVariables(datasetId).then(function (columnsData) {
					var importedData = $scope.importedData[0];
					var newVariables = [];
					var output = [];

					$.each(columnsData, function (i, e) {
						output.push(e.alias)
					});

					for (var i = 0; i < importedData.length; i++) {
						if (!output.includes(importedData[i])) {
							newVariables.push(importedData[i]);
						}
					}

					return newVariables;
				});
			};

			$scope.initMapPopup = function(result, datasetId) {

				// get your angular element
				var elem = angular
					.element('#importMapModal .modal-content[ng-controller=importObservationsCtrl]');

				// get the injector.
				var injector = elem.injector();

				// get the service.
				var myService = injector.get('ImportMappingService');

				myService.datasetId = datasetId;

				var scope = elem.scope();
				scope.datasetId = myService.datasetId;

				// retrieve initial data from the service
				$.getJSON('/Fieldbook/etl/workbook/importObservations/getMappingData/' + result ).done(
					function(data) {

						myService.data = data;
						scope.data = myService.data;

						// apply the changes to the scope.
						scope.$apply();
					});
			};

			$scope.importObservations = function (processWarnings) {
				datasetService.importObservations(datasetId, $rootScope.importedData, processWarnings).then(function () {
					displaySaveSuccessMessage('page-message', 'Your data was successfully imported and may need confirmation.');
					$rootScope.navigateToSubObsTab(datasetId, {reload: true});
					$scope.close();
				}, function (response) {
					if (response.status == 400) {
						showErrorMessage('', response.data.errors[0].message);
					} else if (response.status == 412) {
						ctrl.showConfirmModal(response.data.errors);
					} else {
						showErrorMessage('', ajaxGenericErrorMsg);
					}
				});
			};

			$scope.close = function () {
				$uibModalInstance.close();
			};

			ctrl.showConfirmModal = function (warnings) {
				$uibModalInstance.close();
				var warningMessages = [];
				for (var i = 0; i < warnings.length; i++) {
					warningMessages.push(warnings[i].message);
				}

				var modalWarningMessage = importStudyModalService.showWarningMessage('Confirmation', 'Some observations were found in the imported file:', warningMessages, 'Would you like to proceed with the import ?', 'Proceed', 'Back');
				modalWarningMessage.result.then(function (shouldContinue) {
					if (shouldContinue) {
						$scope.importObservations(false);
					} else {
						importStudyModalService.openImportStudyModal(datasetId);
					}
				});
			};

			ctrl.showDesignMapPopup = function (result, datasetId) {
				$('#importMapModal').one('show.bs.modal', function () {
						$scope.initMapPopup(result, datasetId);
					}).modal();
			};

			ctrl.showAddVariableConfirmModal = function (result, datasetId) {
				$uibModalInstance.close();
				var warningMessages = [];

				var modalWarningMessage = importStudyModalService.showWarningMessage('Confirmation',
					'Some of the variables that you are trying to import are not present in this dataset.', warningMessages,
					'Would you like to add them? ', 'Yes', 'No');
				modalWarningMessage.result.then(function (shouldContinue) {
					if (shouldContinue) {
						ctrl.showDesignMapPopup(result, datasetId);
					} else {
						$scope.importObservations(true);
					}
				});
			};

			ctrl.init = function () {
				$scope.file = null;
				$scope.importedData = null;
				ctrl.format = {selected: ctrl.importFormats[1]};
			};

			ctrl.init();

		}])
		.directive('importObservation', function () {
			return {
				restrict: 'AE',
				scope: {
					importedFile: '=',
					importedData: '='
				},
				link: function (scope, elem, attrs) {
					elem.on('change', function (changeEvent) {
						var reader = new FileReader();

						reader.onload = function (e) {
							/* read workbook */
							var bstr = e.target.result;
							var wb = XLSX.read(bstr, {type: 'binary', raw: true});

							/* grab first sheet */
							var wsname = wb.SheetNames[0];
							if (wb.SheetNames.length > 1) {
								wsname = 'Observation';

								if (!wb.Sheets[wsname]) {
									showErrorMessage('', 'Wrong name for Observation sheet - please remedy in spreadsheet and try again');
									return;
								}
							}
							var ws = wb.Sheets[wsname];

							/* grab first row and generate column headers */
							var aoa = XLSX.utils.sheet_to_json(ws, {header: 1, raw: false, defval: ""});

							/* update scope */
							scope.$apply(function () {
								var length = 20;
								scope.importedData = aoa;
								scope.importedFile = changeEvent.target.files[0];
								scope.importedFile.abbrName = scope.importedFile.name;

								if (scope.importedFile.name.length > length) {
									scope.importedFile.abbrName = scope.importedFile.abbrName.substring(0, length) + '...';
								}

								var fileElement = angular.element('#file_upload');
								angular.element(fileElement).val(null);
							});
						};
						reader.readAsBinaryString(changeEvent.target.files[0]);
					});
				}
			};
		});
})();

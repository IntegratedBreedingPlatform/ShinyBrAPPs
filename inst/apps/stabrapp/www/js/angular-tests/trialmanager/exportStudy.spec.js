'use strict';

describe('Export Study', function () {

	var exportStudyModalService, $httpBackend, $q, serviceUtilities;
	var exportDatasetOptionCtrl, exportDatasetOptionCtrlScope;
	var exportStudyCtrl, exportStudyCtrlScope;
	var $rootScope = jasmine.createSpyObj('$rootScope', ['openConfirmModal']);
	var trialDataManagerService = {
		currentData: {
			environments: {
				noOfEnvironments: 2
			}
		}
	};
	var studyContext = {
		studyId: 1,
		cropName: 'maize',
		measurementDatasetId: 2009
	};
	var $uibModal = jasmine.createSpyObj('$uibModal', ['open']);
	var $uibModalInstance = {
		close: jasmine.createSpy('close'),
		dismiss: jasmine.createSpy('dismiss'),
		result: {
			then: jasmine.createSpy('then')
		}
	};
	var datasetService = jasmine.createSpyObj('datasetService', ['getDatasetInstances', 'exportDataset']);
	var fileDownloadHelper = jasmine.createSpyObj('fileDownloadHelper', ['getFileNameFromResponseContentDisposition', 'save']);

	var datasetId = 2010;
	var instances = [{
		"instanceId": 1,
		"locationName": "CENTER FOR INTERNATIONAL FORESTRY RESEARCH",
		"locationAbbreviation": "CIFOR",
		"instanceNumber": 1,
		"customLocationAbbreviation": "CIF",
		"hasFieldmap": true
	},
		{
			"instanceId": 2,
			"locationName": "Agua Fria",
			"locationAbbreviation": "Agua Fria",
			"instanceNumber": 2,
			"customLocationAbbreviation": "AF",
			"hasFieldmap": true
		}];

	beforeEach(function () {
		module('export-study');
		module(function ($provide) {
			$provide.value("studyContext", studyContext);
			$provide.value("serviceUtilities", {});
			$provide.value("$uibModal", $uibModal);
			$provide.value("$uibModalInstance", $uibModalInstance);
			$provide.value("datasetService", datasetService);
			$provide.value("datasetId", datasetId);
			$provide.value("fileDownloadHelper", fileDownloadHelper);
			$provide.value("TrialManagerDataService", trialDataManagerService);
		});
	});

	beforeEach(function () {
		inject(function ($injector) {
			exportStudyModalService = $injector.get('exportStudyModalService');
			datasetService = $injector.get('datasetService');
			$httpBackend = $injector.get('$httpBackend');
			$uibModal = $injector.get('$uibModal');
			$uibModalInstance = $injector.get('$uibModalInstance');
			$q = $injector.get('$q');

			var $controller = $injector.get('$controller');

			datasetService.getDatasetInstances.and.returnValue($q.resolve(instances));

			// Initialize controllers to be tested.

			exportDatasetOptionCtrlScope = $injector.get('$rootScope').$new();
			exportDatasetOptionCtrl = $controller('exportDatasetOptionCtrl', {
				$rootScope: $rootScope,
				$scope: exportDatasetOptionCtrlScope,
				studyContext: studyContext,
				$uibModal: $uibModal,
				$uibModalInstance: $uibModalInstance,
				exportStudyModalService: exportStudyModalService,
				datasetService: datasetService
			});

			exportStudyCtrlScope = $injector.get('$rootScope').$new();
			exportStudyCtrl = $controller('exportStudyCtrl', {
				$rootScope: $rootScope,
				$scope: exportStudyCtrlScope,
				studyContext: studyContext,
				$uibModalInstance: $uibModalInstance,
				exportStudyModalService: exportStudyModalService,
				datasetService: datasetService,
				fileDownloadHelper: fileDownloadHelper
			});

		})
	});

	describe('Export Study Modal Service', function () {

		describe('openDatasetOptionModal', function () {

			it('should open the Dataset Option modal window', function () {

				exportStudyModalService.openDatasetOptionModal();

				expect($uibModal.open).toHaveBeenCalled();

				var capturedArgument = $uibModal.open.calls.mostRecent().args[0];

				expect(capturedArgument.template).toEqual('<dataset-option-modal modal-title="modalTitle" message="message"' +
					'selected="selected" on-continue="showExportOptions()"></dataset-option-modal>');
				expect(capturedArgument.controller).toEqual('exportDatasetOptionCtrl');
				expect(capturedArgument.size).toEqual('md');

			});

		});

		describe('openExportStudyModal', function () {

			it('should open the Export Study modal window', function () {

				var datasetId = 1;

				exportStudyModalService.openExportStudyModal(datasetId);

				var capturedArgument = $uibModal.open.calls.mostRecent().args[0];

				expect(capturedArgument.templateUrl).toEqual('/Fieldbook/static/angular-templates/exportStudy/exportStudyModal.html');
				expect(capturedArgument.controller).toEqual('exportStudyCtrl');
				expect(capturedArgument.size).toEqual('md');
				expect(capturedArgument.resolve.datasetId()).toEqual(datasetId);
				expect(capturedArgument.controllerAs).toEqual('ctrl');

			});

		});

	});

	describe('Export Dataset Option Modal Controller', function () {

		describe('showExportOptions', function () {

			it('it should open Export Study Modal if selected dataset is SUBOBSERVATION DATASET', function () {

				spyOn(exportStudyModalService, 'openExportStudyModal');

				exportDatasetOptionCtrlScope.selected.datasetId = 999;
				exportDatasetOptionCtrlScope.showExportOptions();

				expect(exportStudyModalService.openExportStudyModal).toHaveBeenCalled();
			});

		});

	});


	describe('Export Study Modal Controller', function () {

		describe('cancel', function () {

			it('it should close the modal instance', function () {
				exportStudyCtrlScope.cancel();
				expect($uibModalInstance.dismiss).toHaveBeenCalled();
			});

		});

		describe('proceed', function () {

			it('it should check instances for fieldmap if the selected collection order is not PLOT_ORDER', function () {

				spyOn(exportStudyCtrl, 'checkIfInstancesHaveFieldMap');
				spyOn(exportStudyCtrl, 'export');

				exportStudyCtrl.selectedCollectionOrderId = '2';
				exportStudyCtrlScope.proceed();

				expect(exportStudyCtrl.checkIfInstancesHaveFieldMap).toHaveBeenCalled();
				expect(exportStudyCtrl.export).not.toHaveBeenCalled();
			});

			it('it should proceed with export if the selected collection order is PLOT_ORDER', function () {

				spyOn(exportStudyCtrl, 'checkIfInstancesHaveFieldMap');
				spyOn(exportStudyCtrl, 'export');

				exportStudyCtrl.selectedCollectionOrderId = '1';
				exportStudyCtrlScope.proceed();

				expect(exportStudyCtrl.checkIfInstancesHaveFieldMap).not.toHaveBeenCalled();
				expect(exportStudyCtrl.export).toHaveBeenCalled();
			});

		});

		describe('getSelectedInstanceIds', function () {

			it('it should retrieve the instanceIds of the selected instances', function () {
				exportStudyCtrlScope.selectedInstances = {
					1: true,
					2: false,
					3: true,
					4: false
				};

				var instanceIds = exportStudyCtrl.getSelectedInstanceIds();
				expect(instanceIds).toEqual(['1', '3']);

			});

		});

		describe('checkIfInstancesHaveFieldMap', function () {

			it('it should show the confirm modal if any of the selected instances has no field map', function () {

				var instanceIds = ['1', '2'];
				exportStudyCtrlScope.instances = instances;
				exportStudyCtrlScope.instances[0].hasFieldmap = true;
				exportStudyCtrlScope.instances[1].hasFieldmap = false;

				spyOn(exportStudyCtrl, 'showConfirmModal');
				spyOn(exportStudyCtrl, 'export');

				exportStudyCtrl.checkIfInstancesHaveFieldMap(instanceIds);

				expect(exportStudyCtrl.showConfirmModal).toHaveBeenCalled();
				expect(exportStudyCtrl.export).not.toHaveBeenCalled();

			});

			it('it should immediately export if all of the selected instances has field map', function () {

				var instanceIds = ['1', '2'];
				exportStudyCtrlScope.instances = instances;
				exportStudyCtrlScope.instances[0].hasFieldmap = true;
				exportStudyCtrlScope.instances[1].hasFieldmap = true;

				spyOn(exportStudyCtrl, 'showConfirmModal');
				spyOn(exportStudyCtrl, 'export');

				exportStudyCtrl.checkIfInstancesHaveFieldMap(instanceIds);

				expect(exportStudyCtrl.export).toHaveBeenCalled();
				expect(exportStudyCtrl.showConfirmModal).not.toHaveBeenCalled();

			});

		});

		describe('showConfirmModal', function () {

			it('it should show the confirm modal and if the user click proceed, do the export', function () {

				var instanceIds = ['1', '2'];

				spyOn(exportStudyCtrl, 'export');
				$rootScope.openConfirmModal.and.returnValue($uibModalInstance);

				exportStudyCtrl.showConfirmModal(instanceIds);

				expect($rootScope.openConfirmModal).toHaveBeenCalled();
				expect($uibModalInstance.result.then).toHaveBeenCalled();

				var modalCloseCallback = $uibModalInstance.result.then.calls.mostRecent().args[0];

				modalCloseCallback(true);
				expect(exportStudyCtrl.export).toHaveBeenCalled();

			});

			it('it should show the confirm modal and if the user click cancel, do not export', function () {

				var instanceIds = ['1', '2'];

				spyOn(exportStudyCtrl, 'export');
				$rootScope.openConfirmModal.and.returnValue($uibModalInstance);

				exportStudyCtrl.showConfirmModal(instanceIds);

				expect($rootScope.openConfirmModal).toHaveBeenCalled();
				expect($uibModalInstance.result.then).toHaveBeenCalled();

				var modalCloseCallback = $uibModalInstance.result.then.calls.mostRecent().args[0];

				modalCloseCallback(false);
				expect(exportStudyCtrl.export).not.toHaveBeenCalled();

			});

		});

		describe('export', function () {

			it('it should generate and download the exported file', function () {

				var instanceIds = ['1', '2'];
				var response = {data: {}};
				var fileName = 'test.csv';

				datasetService.exportDataset.and.returnValue($q.resolve(response));
				fileDownloadHelper.getFileNameFromResponseContentDisposition.and.returnValue(fileName);

				exportStudyCtrl.export(instanceIds);

				exportStudyCtrlScope.$apply();

				expect(fileDownloadHelper.getFileNameFromResponseContentDisposition).toHaveBeenCalledWith(response);
				expect(fileDownloadHelper.save).toHaveBeenCalledWith(response.data, fileName);
				expect($uibModalInstance.close).toHaveBeenCalled();

			});

		});

	});

});

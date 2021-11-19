'use strict';

describe('Generate Design Controller', function () {
	var generateDesignCtrl, scope, $q;
	var rootScope = jasmine.createSpyObj('$rootScope', ['openConfirmModal']);
	var experimentDesignService = jasmine.createSpyObj('experimentDesignService', ['generateDesign']);
	var studyContext = {
		studyId: 1,
		cropName: 'maize',
		measurementDatasetId: 2009
	};
	var studyInstances;

	var uibModalInstance = {
		close: jasmine.createSpy('close'),
		dismiss: jasmine.createSpy('dismiss'),
		result: {
			then: jasmine.createSpy('then')
		}
	};

	var experimentDesignInput = {};

	beforeEach(function () {
		module('manageTrialApp');
		module(function ($provide) {
			$provide.value("experimentDesignService", experimentDesignService);
		});

		studyInstances = [
			{
				instanceNumber: 1,
				hasMeasurements: false,
				hasFieldmap: false,
				hasExperimentalDesign: false,
			},
		];
	});

	beforeEach(function () {
		inject(function ($injector) {
			scope = $injector.get('$rootScope').$new();
			var $controller = $injector.get('$controller');
			$q = $injector.get('$q');
			experimentDesignService = $injector.get('experimentDesignService');
			generateDesignCtrl = $controller('generateDesignCtrl', {
				experimentDesignInput: experimentDesignInput,
				studyInstances: studyInstances,
				$scope: scope,
				$rootScope: rootScope,
				$uibModalInstance: uibModalInstance,
				experimentDesignService: experimentDesignService,
				studyContext: studyContext,
			});

			scope.selectedInstances = {
				1: true,
			};

			scope.instances = studyInstances;
		})
	});

	describe('Validate Selected Environments', function () {
		describe('validateSelectedEnvironments', function () {
			it('should generate the design', function () {
				rootScope.openConfirmModal.and.returnValue(uibModalInstance);
				var response = {data: {}};
				experimentDesignService.generateDesign.and.returnValue($q.resolve(response));
				spyOn(generateDesignCtrl, 'generateDesign');
				scope.validateSelectedEnvironments();
				expect(generateDesignCtrl.generateDesign).toHaveBeenCalled();
			});
		});
		describe('validateSelectedEnvironments', function () {
			it('should show has measurements warning', function () {
				rootScope.openConfirmModal.and.returnValue(uibModalInstance);
				scope.instances[0].hasMeasurements = true;
				spyOn(generateDesignCtrl, 'generateDesign');
				scope.validateSelectedEnvironments();
				expect(rootScope.openConfirmModal).toHaveBeenCalled();
				expect(generateDesignCtrl.generateDesign).not.toHaveBeenCalled();
			});
		});
		describe('validateSelectedEnvironments', function () {
			it('should show has generated design warning', function () {
				rootScope.openConfirmModal.and.returnValue(uibModalInstance);
				scope.instances[0].hasExperimentalDesign = true;
				spyOn(generateDesignCtrl, 'generateDesign');
				scope.validateSelectedEnvironments();
				expect(rootScope.openConfirmModal).toHaveBeenCalled();
				expect(generateDesignCtrl.generateDesign).not.toHaveBeenCalled();
			});
		});
	});

});

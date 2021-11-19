'use strict';

describe('PlantingPreparationModalCtrl:', function () {
	var $controller,
		$rootScope,
		$q,
		$timeout,
		controller,
		scope;

	// Mocks
	var studyContextMock = {
			studyId: 1,
			cropName: 'maize',
			measurementDatasetId: 2009
		},
		uibModalInstanceMock = {
			close: jasmine.createSpy('close'),
			dismiss: jasmine.createSpy('dismiss'),
			result: {
				then: jasmine.createSpy('then')
			}
		},
		InventoryServiceMock = jasmine.createSpyObj('InventoryService', [
			'queryUnits'
		]),
		PlantingPreparationServiceMock = jasmine.createSpyObj('datasetService', [
			'getPlantingPreparationData',
			'getMetadata',
			'confirmPlanting'
		]),
		HasAnyAuthorityServiceMock = {},
		PERMISSIONSMock	= [];

	beforeEach(function (done) {
		module('manageTrialApp');

		module(function ($provide) {
			$provide.value("PlantingPreparationService", PlantingPreparationServiceMock);
			$provide.value("InventoryService", InventoryServiceMock);
		});

		inject(function(_$controller_, _$rootScope_, _$q_, $injector, _$timeout_, $httpBackend) {
			$controller = _$controller_;
			$rootScope = _$rootScope_;
			$timeout = _$timeout_;
			$q = _$q_;

			scope = $rootScope.$new();
			scope.$resolve = {
				searchComposite: {
					itemIds: [123501, 123505]
				},
				datasetId: 1
			};

			$httpBackend.whenGET('/Fieldbook/TrialManager/createTrial/trialSettings').respond(200, {data: "ok"});

			PlantingPreparationServiceMock.getPlantingPreparationData.and.returnValue($q.resolve(getMockPlantingPreparationData()))
			InventoryServiceMock.queryUnits.and.returnValue($q.resolve(getMockUnits()))

			controller = $controller('PlantingPreparationModalCtrl', {
				$scope: scope,
				$uibModalInstance: uibModalInstanceMock,
				service: PlantingPreparationServiceMock,
				InventoryService: InventoryServiceMock,
				studyContext: studyContextMock,
				HasAnyAuthorityService: HasAnyAuthorityServiceMock,
				PERMISSIONS: PERMISSIONSMock
			});

			scope.initPromise.then(function () {
				done();
			});

			$rootScope.$apply();
		});
	});

	describe('an entry', function () {
		it('should pass validation when availableBalance > withdrawal', function () {
			const entry = scope.entryMap[8264][9];
			entry.stockSelected = entry.stockByStockId['SID1-8']
			scope.units[entry.stockSelected.unitId].amountPerPacket = 50;
			expect(scope.isValid(entry)).toBe(false);

			scope.units[entry.stockSelected.unitId].amountPerPacket = 44;
			expect(scope.isValid(entry)).toBe(true);
		});
	})

	function getMockPlantingPreparationData() {
		return {
			"entries": [
				{
					"entryNo": 9,
					"entryType": "Test entry",
					"gid": 8,
					"designation": "CML8",
					"stockByStockId": {
						"SID1-8": {
							"stockId": "SID1-8",
							"lotId": 8,
							"storageLocation": "Default Seed Store",
							"availableBalance": 45,
							"unitId": 8264
						},
						"ICRAF0": {
							"stockId": "ICRAF0",
							"lotId": 407142,
							"storageLocation": "INT CENTER FOR RESEARCH IN AGROFORSTRY",
							"availableBalance": 1,
							"unitId": 8267
						},
						"ICRAF2": {
							"stockId": "ICRAF2",
							"lotId": 407144,
							"storageLocation": "INT CENTER FOR RESEARCH IN AGROFORSTRY",
							"availableBalance": 1,
							"unitId": 8268
						}
					},
					"observationUnits": [
						{
							"ndExperimentId": 123501,
							"observationUnitId": "b3afda17-0106-4a5f-8af7-f3eeb9e4ecea",
							"instanceId": 1
						}
					]
				},
				{
					"entryNo": 1,
					"entryType": "Test entry",
					"gid": 1,
					"designation": "CML1",
					"stockByStockId": {
						"SID1-1": {
							"stockId": "SID1-1",
							"lotId": 1,
							"storageLocation": "INT CENTER FOR RESEARCH IN AGROFORSTRY",
							"availableBalance": 20,
							"unitId": 8264
						}
					},
					"observationUnits": [
						{
							"ndExperimentId": 123505,
							"observationUnitId": "e46a9d13-f380-4035-98d8-1a633dcb92df",
							"instanceId": 1
						}
					]
				}
			]
		}
	}

	function getMockUnits() {
		return [
			{
				"id": "8264",
				"name": "SEED_AMOUNT_g",
			},
			{
				"id": "8267",
				"name": "SEED_AMOUNT_kg",
			},
			{
				"id": "8266",
				"name": "SEED_AMOUNT_No",
			},
			{
				"id": "8268",
				"name": "SEED_AMOUNT_Packets",
			},
			{
				"id": "8710",
				"name": "SEED_AMOUNT_t",
			}
		];
	}
});

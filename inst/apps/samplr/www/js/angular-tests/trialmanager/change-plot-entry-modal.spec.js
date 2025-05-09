'use strict';

describe('ChangePlotEntryModalCtrl:', function () {
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
		dTOptionsBuilderMock = {},
		dTColumnBuilderMock = {},
		$compileMock = {},
		ChangePlotEntryServiceMock = jasmine.createSpyObj('ChangePlotEntryService', [
			'getColumns',
			'getEntriesTableUrl',
			'updateObservationUnitsEntry'
		]),
		columns = [
			{
				"termId": 8255,
				"name": "ENTRY_TYPE",
				"alias": "ENTRY_TYPE",
				"description": "Entry type (test/check)- assigned (type)",
				"scale": "Type of ENTRY_TYPE",
				"method": "Assigned",
				"property": "Entry type",
				"dataType": "Categorical",
				"value": null,
				"label": "",
				"dataTypeId": 1130,
				"possibleValues": [
					{
						"id": 10170,
						"name": "T",
						"description": "Test entry",
						"programUUID": null,
						"key": "10170",
						"displayDescription": "T= Test entry",
						"folder": false,
						"study": false
					},
					{
						"id": 10180,
						"name": "C",
						"description": "Check entry",
						"programUUID": null,
						"key": "10180",
						"displayDescription": "C= Check entry",
						"folder": false,
						"study": false
					},
					{
						"id": 10190,
						"name": "D",
						"description": "Disease check",
						"programUUID": null,
						"key": "10190",
						"displayDescription": "D= Disease check",
						"folder": false,
						"study": false
					},
					{
						"id": 10200,
						"name": "S",
						"description": "Stress check",
						"programUUID": null,
						"key": "10200",
						"displayDescription": "S= Stress check",
						"folder": false,
						"study": false
					}
				],
				"possibleValuesString": "",
				"minRange": null,
				"maxRange": null,
				"scaleMinRange": null,
				"scaleMaxRange": null,
				"variableMinRange": null,
				"variableMaxRange": null,
				"required": false,
				"treatmentLabel": null,
				"operation": null,
				"role": null,
				"variableType": "GERMPLASM_DESCRIPTOR",
				"formula": null,
				"cropOntology": null,
				"factor": true,
				"dataTypeCode": "C"
			},
			{
				"termId": 8240,
				"name": "GID",
				"alias": "GID",
				"description": "Germplasm identifier - assigned (DBID)",
				"scale": "Germplasm id",
				"method": "Assigned",
				"property": "Germplasm id",
				"dataType": "Germplasm List",
				"value": null,
				"label": "",
				"dataTypeId": 1135,
				"possibleValues": null,
				"possibleValuesString": null,
				"minRange": null,
				"maxRange": null,
				"scaleMinRange": null,
				"scaleMaxRange": null,
				"variableMinRange": null,
				"variableMaxRange": null,
				"required": false,
				"treatmentLabel": null,
				"operation": null,
				"role": null,
				"variableType": "GERMPLASM_DESCRIPTOR",
				"formula": null,
				"cropOntology": null,
				"factor": true,
				"dataTypeCode": "C"
			},
			{
				"termId": 8250,
				"name": "DESIGNATION",
				"alias": "DESIGNATION",
				"description": "Germplasm identifier - assigned (DBCV)",
				"scale": "Germplasm name",
				"method": "Assigned",
				"property": "Germplasm id",
				"dataType": "Germplasm List",
				"value": null,
				"label": "",
				"dataTypeId": 1135,
				"possibleValues": null,
				"possibleValuesString": null,
				"minRange": null,
				"maxRange": null,
				"scaleMinRange": null,
				"scaleMaxRange": null,
				"variableMinRange": null,
				"variableMaxRange": null,
				"required": false,
				"treatmentLabel": null,
				"operation": null,
				"role": null,
				"variableType": "GERMPLASM_DESCRIPTOR",
				"formula": null,
				"cropOntology": null,
				"factor": true,
				"dataTypeCode": "C"
			},
			{
				"termId": 8230,
				"name": "ENTRY_NO",
				"alias": "ENTRY_NO",
				"description": "Germplasm entry - enumerated (number)",
				"scale": "Number",
				"method": "Enumerated",
				"property": "Germplasm entry",
				"dataType": "Numeric",
				"value": null,
				"label": "",
				"dataTypeId": 1110,
				"possibleValues": null,
				"possibleValuesString": null,
				"minRange": null,
				"maxRange": null,
				"scaleMinRange": null,
				"scaleMaxRange": null,
				"variableMinRange": null,
				"variableMaxRange": null,
				"required": false,
				"treatmentLabel": null,
				"operation": null,
				"role": null,
				"variableType": "GERMPLASM_DESCRIPTOR",
				"formula": null,
				"cropOntology": null,
				"factor": true,
				"dataTypeCode": "N"
			},
			{
				"termId": -3,
				"name": "LOTS",
				"alias": "LOTS",
				"description": null,
				"scale": null,
				"method": null,
				"property": null,
				"dataType": null,
				"value": null,
				"label": "",
				"dataTypeId": null,
				"possibleValues": null,
				"possibleValuesString": null,
				"minRange": null,
				"maxRange": null,
				"scaleMinRange": null,
				"scaleMaxRange": null,
				"variableMinRange": null,
				"variableMaxRange": null,
				"required": false,
				"treatmentLabel": null,
				"operation": null,
				"role": null,
				"variableType": null,
				"formula": null,
				"cropOntology": null,
				"factor": true,
				"dataTypeCode": ""
			},
			{
				"termId": -4,
				"name": "AVAILABLE",
				"alias": "AVAILABLE",
				"description": null,
				"scale": null,
				"method": null,
				"property": null,
				"dataType": null,
				"value": null,
				"label": "",
				"dataTypeId": null,
				"possibleValues": null,
				"possibleValuesString": null,
				"minRange": null,
				"maxRange": null,
				"scaleMinRange": null,
				"scaleMaxRange": null,
				"variableMinRange": null,
				"variableMaxRange": null,
				"required": false,
				"treatmentLabel": null,
				"operation": null,
				"role": null,
				"variableType": null,
				"formula": null,
				"cropOntology": null,
				"factor": true,
				"dataTypeCode": ""
			},
			{
				"termId": -5,
				"name": "UNIT",
				"alias": "UNIT",
				"description": null,
				"scale": null,
				"method": null,
				"property": null,
				"dataType": null,
				"value": null,
				"label": "",
				"dataTypeId": null,
				"possibleValues": null,
				"possibleValuesString": null,
				"minRange": null,
				"maxRange": null,
				"scaleMinRange": null,
				"scaleMaxRange": null,
				"variableMinRange": null,
				"variableMaxRange": null,
				"required": false,
				"treatmentLabel": null,
				"operation": null,
				"role": null,
				"variableType": null,
				"formula": null,
				"cropOntology": null,
				"factor": true,
				"dataTypeCode": ""
			}
		],
		columnData = [
			{
				"entryId": 1,
				"entryNumber": 1,
				"entryCode": "1",
				"gid": 1,
				"designation": "SEVERAL001",
				"lots": 1,
				"available": "102",
				"unit": "SEED_AMOUNT_kg",
				"variables": {
					"GID": {
						"value": "1"
					},
					"ENTRY_NO": {
						"value": "1"
					},
					"DESIGNATION": {
						"value": "SEVERAL001"
					},
					"ENTRY_CODE": {
						"value": "1"
					},
					"ENTRY_TYPE": {
						"studyEntryPropertyId": 2,
						"variableId": 8255,
						"value": "10170"
					}
				}
			},
			{
				"entryId": 2,
				"entryNumber": 2,
				"entryCode": "2",
				"gid": 2,
				"designation": "SEVERAL002",
				"lots": 1,
				"available": "102",
				"unit": "SEED_AMOUNT_kg",
				"variables": {
					"GID": {
						"value": "2"
					},
					"ENTRY_NO": {
						"value": "2"
					},
					"DESIGNATION": {
						"value": "SEVERAL002"
					},
					"ENTRY_CODE": {
						"value": "2"
					},
					"ENTRY_TYPE": {
						"studyEntryPropertyId": 6,
						"variableId": 8255,
						"value": "10170"
					}
				}
			},
			{
				"entryId": 3,
				"entryNumber": 3,
				"entryCode": "3",
				"gid": 3,
				"designation": "SEVERAL003",
				"lots": 1,
				"available": "102",
				"unit": "SEED_AMOUNT_kg",
				"variables": {
					"GID": {
						"value": "3"
					},
					"ENTRY_NO": {
						"value": "3"
					},
					"DESIGNATION": {
						"value": "SEVERAL003"
					},
					"ENTRY_CODE": {
						"value": "3"
					},
					"ENTRY_TYPE": {
						"studyEntryPropertyId": 11,
						"variableId": 8255,
						"value": "10170"
					}
				}
			},
			{
				"entryId": 4,
				"entryNumber": 4,
				"entryCode": "4",
				"gid": 4,
				"designation": "SEVERAL004",
				"lots": 1,
				"available": "102",
				"unit": "SEED_AMOUNT_kg",
				"variables": {
					"GID": {
						"value": "4"
					},
					"ENTRY_NO": {
						"value": "4"
					},
					"DESIGNATION": {
						"value": "SEVERAL004"
					},
					"ENTRY_CODE": {
						"value": "4"
					},
					"ENTRY_TYPE": {
						"studyEntryPropertyId": 13,
						"variableId": 8255,
						"value": "10170"
					}
				}
			},
			{
				"entryId": 5,
				"entryNumber": 5,
				"entryCode": "5",
				"gid": 5,
				"designation": "SEVERAL005",
				"lots": 1,
				"available": "102",
				"unit": "SEED_AMOUNT_kg",
				"variables": {
					"GID": {
						"value": "5"
					},
					"ENTRY_NO": {
						"value": "5"
					},
					"DESIGNATION": {
						"value": "SEVERAL005"
					},
					"ENTRY_CODE": {
						"value": "5"
					},
					"ENTRY_TYPE": {
						"studyEntryPropertyId": 17,
						"variableId": 8255,
						"value": "10170"
					}
				}
			},
			{
				"entryId": 6,
				"entryNumber": 6,
				"entryCode": "6",
				"gid": 6,
				"designation": "SEVERAL006",
				"lots": 1,
				"available": "102",
				"unit": "SEED_AMOUNT_kg",
				"variables": {
					"GID": {
						"value": "6"
					},
					"ENTRY_NO": {
						"value": "6"
					},
					"DESIGNATION": {
						"value": "SEVERAL006"
					},
					"ENTRY_CODE": {
						"value": "6"
					},
					"ENTRY_TYPE": {
						"studyEntryPropertyId": 24,
						"variableId": 8255,
						"value": "10170"
					}
				}
			},
			{
				"entryId": 7,
				"entryNumber": 7,
				"entryCode": "7",
				"gid": 7,
				"designation": "SEVERAL007",
				"lots": 1,
				"available": "102",
				"unit": "SEED_AMOUNT_kg",
				"variables": {
					"GID": {
						"value": "7"
					},
					"ENTRY_NO": {
						"value": "7"
					},
					"DESIGNATION": {
						"value": "SEVERAL007"
					},
					"ENTRY_CODE": {
						"value": "7"
					},
					"ENTRY_TYPE": {
						"studyEntryPropertyId": 26,
						"variableId": 8255,
						"value": "10170"
					}
				}
			},
			{
				"entryId": 8,
				"entryNumber": 8,
				"entryCode": "8",
				"gid": 8,
				"designation": "SEVERAL008",
				"lots": 1,
				"available": "102",
				"unit": "SEED_AMOUNT_kg",
				"variables": {
					"GID": {
						"value": "8"
					},
					"ENTRY_NO": {
						"value": "8"
					},
					"DESIGNATION": {
						"value": "SEVERAL008"
					},
					"ENTRY_CODE": {
						"value": "8"
					},
					"ENTRY_TYPE": {
						"studyEntryPropertyId": 30,
						"variableId": 8255,
						"value": "10170"
					}
				}
			},
			{
				"entryId": 9,
				"entryNumber": 9,
				"entryCode": "9",
				"gid": 9,
				"designation": "SEVERAL009",
				"lots": 1,
				"available": "102",
				"unit": "SEED_AMOUNT_kg",
				"variables": {
					"GID": {
						"value": "9"
					},
					"ENTRY_NO": {
						"value": "9"
					},
					"DESIGNATION": {
						"value": "SEVERAL009"
					},
					"ENTRY_CODE": {
						"value": "9"
					},
					"ENTRY_TYPE": {
						"studyEntryPropertyId": 34,
						"variableId": 8255,
						"value": "10170"
					}
				}
			},
			{
				"entryId": 10,
				"entryNumber": 10,
				"entryCode": "10",
				"gid": 10,
				"designation": "SEVERAL010",
				"lots": 1,
				"available": "102",
				"unit": "SEED_AMOUNT_kg",
				"variables": {
					"GID": {
						"value": "10"
					},
					"ENTRY_NO": {
						"value": "10"
					},
					"DESIGNATION": {
						"value": "SEVERAL010"
					},
					"ENTRY_CODE": {
						"value": "10"
					},
					"ENTRY_TYPE": {
						"studyEntryPropertyId": 37,
						"variableId": 8255,
						"value": "10170"
					}
				}
			}
		]

	beforeEach(function (done) {
		module('manageTrialApp');

		module(function ($provide) {
			$provide.value("ChangePlotEntryService", ChangePlotEntryServiceMock);
			$provide.value("studyContext", studyContextMock);
		});

		inject(function (_$controller_, _$rootScope_, _$q_, $injector, _$timeout_, $httpBackend) {
			$controller = _$controller_;
			$rootScope = _$rootScope_;
			$timeout = _$timeout_;
			$q = _$q_;

			scope = $rootScope.$new();
			scope.$resolve = {
				searchComposite: {
					itemIds: [7501, 7500]
				},
				datasetId: 2009,
				numberOfInstances: 1,
				numberOfPlots: 2
			};

			$httpBackend.whenGET('/Fieldbook/TrialManager/createTrial/trialSettings').respond(200, {data: "ok"});


			ChangePlotEntryServiceMock.getColumns.and.returnValue($q.resolve(columns))
			ChangePlotEntryServiceMock.getEntriesTableUrl.and.returnValue($q.resolve(columnData))
			ChangePlotEntryServiceMock.updateObservationUnitsEntry.and.returnValue($q.resolve(''))
			controller = $controller('ChangePlotEntryModalCtrl', {
				$scope: scope,
				$rootScope: $rootScope,
				$uibModalInstance: uibModalInstanceMock,
				dTOptionsBuilder: dTOptionsBuilderMock,
				dTColumnBuilder: dTColumnBuilderMock,
				ChangePlotEntryService: ChangePlotEntryServiceMock,
				$compile: $compileMock,
				$timeout: $timeout,
				studyContext: studyContextMock
			});

			scope.initPromise.then(function () {
				done();
			});

			$rootScope.$apply();
		});
	});

	describe('Change plot entry', function () {
		it('should initialize correctly', function () {
			expect(scope.$resolve.numberOfInstances).toBe(1);
			expect(scope.numberOfInstances).toBe(1);

			expect(scope.$resolve.numberOfPlots).toBe(2);
			expect(scope.numberOfPlots).toBe(2);

			expect(scope.selected.entryId).toBe('');

			expect(scope.$resolve.datasetId).toBe(2009);
			expect(scope.$resolve.searchComposite.itemIds.length).toBe(2);
			expect(scope.$resolve.searchComposite.itemIds).toEqual([7501, 7500]);

			spyOn(scope, 'confirm');

			expect(scope.confirm).not.toHaveBeenCalled();

		});

		if('should confirm change plot entry', function () {
			expect(ChangePlotEntryService.updateObservationUnitsEntry).toHaveBeenCalled();
			scope.selected.entryId = 1;

			scope.confirm();
			expect(scope.selected.entryId).toBe(1);
			spyOn(scope, "proceed").and.callFake(function() {
				return true;
			});
			expect(ChangePlotEntryService.updateObservationUnitsEntry).toHaveBeenCalled();
		});
	})

});

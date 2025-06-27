'use strict';

describe('Study Entry Service', function () {

	var studyEntryService, $httpBackend, $q;
	var studyContext = {
		studyId: 1,
		cropName: 'maize',
		measurementDatasetId: 2009
	};
	var entryId = 55;
	var newGid = 23;
	var $uibModal = jasmine.createSpyObj('$uibModal', ['open']);

	beforeEach(function () {

		module('manageTrialApp');

		module(function ($provide) {
			$provide.value("studyContext", studyContext);
			$provide.value("serviceUtilities", {});
			$provide.value("$uibModal", $uibModal);
		});
	});

	beforeEach(inject(function ($injector) {
		studyEntryService = $injector.get('studyEntryService');
		$httpBackend = $injector.get('$httpBackend');
		$q = $injector.get('$q');
		$uibModal = $injector.get('$uibModal');
	}));

	describe('Replace Germplasm API call', function () {

		it('should call the correct web api', function () {
			$httpBackend.whenPUT('/bmsapi/crops/maize/programs/' + studyContext.programId + '/studies/' + studyContext.studyId + '/germplasm/' +
				entryId)
				.respond(200, {data: getMockStudyGermplasm()});

			studyEntryService.replaceStudyGermplasm(entryId, newGid).then(function (response) {
				expect(response.data).toEqual(mockData);
			});

		});

		it('should return reject if any of the parameters are undefined', function () {

			studyEntryService.replaceStudyGermplasm(undefined, undefined).then(function (response) {
			}).catch(function (reason) {
				expect(reason).toEqual('studyId, datasetId and variableIds are not defined.');
			});

		});

	});

	function getMockStudyGermplasm() {
		return {
			entryId : 56,
			germplasmId : 23
		};
	}


});

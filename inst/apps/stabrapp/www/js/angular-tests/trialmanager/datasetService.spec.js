'use strict';

describe('Dataset Service', function () {

	var datasetService, $httpBackend, $q;
	var studyContext = {
		studyId: 1,
		cropName: 'maize',
		measurementDatasetId: 2009
	};

	beforeEach(function () {

		module('datasets-api');

		module(function ($provide) {
			$provide.value("studyContext", studyContext);
			$provide.value("serviceUtilities", {});
			$provide.value("DATASET_TYPES_OBSERVATION_IDS", [4, 5, 6, 7, 8]);
			$provide.value("DATASET_TYPES", {});
		});
	});

	beforeEach(inject(function ($injector) {
		datasetService = $injector.get('datasetService');
		$httpBackend = $injector.get('$httpBackend');
		$q = $injector.get('$q');
	}));

	describe('observationCount', function () {

		it('should call the correct web api', function () {

			var studyId = 1;
			var datasetId = 2;
			var variableIds = [888, 999];

			$httpBackend.whenHEAD('/bmsapi/crops/maize/studies/' + studyId + '/datasets/' +
				datasetId + '/variables/observations?variableIds=' + variableIds.join(','))
				.respond({}, {'X-Total-Count': '100'});

			datasetService.observationCount(datasetId, variableIds).then(function (response) {
				expect(response.headers('X-Total-Count')).toEqual('100');
			});

		});

		it('should return reject if any of the parameters are undefined', function () {

			datasetService.observationCount(undefined, undefined).then(function (response) {
			}).catch(function (reason) {
				expect(reason).toEqual('studyId, datasetId and variableIds are not defined.');
			});

		});

	});

	describe('observationCountByInstance', function () {

		it('should call the correct web api', function () {

			var studyId = 1;
			var datasetId = 2;
			var instanceId = 3;

			$httpBackend.whenHEAD('/bmsapi/crops/maize/studies/' + studyId + '/datasets/' +
				datasetId + '/observationUnits/' + instanceId)
				.respond({}, {'X-Total-Count': '200'});

			datasetService.observationCountByInstance(datasetId, instanceId).then(function (response) {
				expect(response.headers('X-Total-Count')).toEqual('200');
			});

		});

		it('should return reject if any of the parameters are undefined', function () {

			datasetService.observationCountByInstance(undefined, undefined).then(function (response) {
			}).catch(function (reason) {
				expect(reason).toEqual('studyId, instanceId and datasetId are not defined.');
			});

		});

	});


	describe('getDatasetInstances', function () {

		it('should call the correct web api', function () {

			var datasetId = 2;
			var instanceId = 3;
			var mockData = {};

			$httpBackend.whenGET('/bmsapi/crops/maize/studies/' + studyContext.studyId + '/datasets/' +
				datasetId + '/instances')
				.respond(mockData);

			datasetService.getDatasetInstances(datasetId).then(function (response) {
				expect(response.data).toEqual(mockData);
			});

		});

	});

	describe('exportDataset', function () {

		it('should call the correct web api', function () {

			var datasetId = 2;
			var instanceIds = [1, 2, 3];
			var collectionOrderId = 1;
			var mockData = {};

			$httpBackend.whenGET('/bmsapi/crops/maize/studies/' + studyContext.studyId + '/datasets/' +
				datasetId + '/csv/')
				.respond(mockData);

			datasetService.exportDataset(datasetId, instanceIds, collectionOrderId).then(function (response) {
				expect(response.data).toEqual(mockData);
			});


		});

	});

	describe('getObservationUnitsMetadata', function () {

		it('should call the correct web api', function () {
			var datasetId = 2;
			var searchCompositeRequest = {};
			var mockData = {};

			$httpBackend.whenGET('/bmsapi/crops/maize/studies/' + studyContext.studyId + '/datasets/' +
				datasetId + '/observation-units/metadata')
				.respond(mockData);

			datasetService.getObservationUnitsMetadata(searchCompositeRequest ,datasetId).then(function (response) {
				expect(response.data).toEqual(mockData);
			});

		});

	});

});

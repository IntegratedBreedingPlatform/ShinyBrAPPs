/*global angular*/

(function () {
	'use strict';

	var manageTrialApp = angular.module('manageTrialApp');

	manageTrialApp.factory('studyInstanceService', ['$rootScope', 'TrialManagerDataService', '$http', 'serviceUtilities', 'studyContext', function ($rootScope, TrialManagerDataService, $http, serviceUtilities, studyContext) {

		var BASE_STUDY_URL = '/bmsapi/crops/' + studyContext.cropName + '/programs/' + studyContext.programId + '/studies/';

		var studyInstanceService = {};
		var successHandler = serviceUtilities.restSuccessHandler,
			failureHandler = serviceUtilities.restFailureHandler;

		studyInstanceService.createStudyInstances = function (numberOfInstancesToGenerate) {
			var request = $http.post(BASE_STUDY_URL + studyContext.studyId + '/instances/generation/?numberOfInstancesToGenerate=' + numberOfInstancesToGenerate);
			return request.then(successHandler, failureHandler);
		}

		studyInstanceService.getStudyInstances = function () {
			var request = $http.get(BASE_STUDY_URL + studyContext.studyId + '/instances');
			return request.then(successHandler, failureHandler);
		};

		studyInstanceService.getStudyInstance = function (instanceId) {
			var request = $http.get(BASE_STUDY_URL + studyContext.studyId + '/instances/' + instanceId);
			return request.then(successHandler, failureHandler);
		};

		studyInstanceService.deleteStudyInstances = function (instanceIds) {
			var request = $http.delete(BASE_STUDY_URL + studyContext.studyId + '/instances/?instanceIds=' + instanceIds.join(','));
			return request.then(successHandler, failureHandler);
		};

		studyInstanceService.addInstanceObservation = function (instanceObservationData) {
			var request = $http.post(BASE_STUDY_URL + studyContext.studyId + '/instances/' + instanceObservationData.instanceId + '/observations', instanceObservationData);
			return request.then(successHandler, failureHandler);
		};

		studyInstanceService.updateInstanceObservation = function (instanceObservationData) {
			var request = $http.patch(BASE_STUDY_URL + studyContext.studyId + '/instances/' + instanceObservationData.instanceId + '/observations/' + instanceObservationData.instanceObservationId, instanceObservationData);
			return request.then(successHandler, failureHandler);
		};

		studyInstanceService.addInstanceDescriptorData = function (instanceDescriptorData) {
			var request = $http.post(BASE_STUDY_URL + studyContext.studyId + '/instances/' + instanceDescriptorData.instanceId + '/descriptors', instanceDescriptorData);
			return request.then(successHandler, failureHandler);
		};

		studyInstanceService.updateInstanceDescriptorData = function (instanceDescriptorData) {
			var request = $http.patch(BASE_STUDY_URL + studyContext.studyId + '/instances/' + instanceDescriptorData.instanceId + '/descriptors/' + instanceDescriptorData.instanceDescriptorDataId, instanceDescriptorData);
			return request.then(successHandler, failureHandler);
		};

		studyInstanceService.instanceInfo = TrialManagerDataService.currentData.instanceInfo;

		studyInstanceService.changeEnvironments = function () {
			this.broadcastEnvironments();
		};

		studyInstanceService.broadcastEnvironments = function () {
			$rootScope.$broadcast('changeEnvironments');
		};

		return studyInstanceService;

	}]);
})();

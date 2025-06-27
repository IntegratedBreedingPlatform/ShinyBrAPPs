/*global angular */

(function () {
    'use strict';

    var manageTrialAppModule = angular.module('manageTrialApp');

    manageTrialAppModule.factory('studyEntryService', ['$http', 'serviceUtilities', '$uibModal', 'studyContext', function ($http, serviceUtilities, $uibModal, studyContext) {

		var BASE_PROGRAM_URL = '/bmsapi/crops/' + studyContext.cropName + '/programs/' + studyContext.programId;
        var BASE_STUDY_URL = BASE_PROGRAM_URL + '/studies/';

        var studyEntryService = {};

        var successHandler = serviceUtilities.restSuccessHandler,
            failureHandler = serviceUtilities.restFailureHandler;

        studyEntryService.replaceStudyGermplasm = function (entryId, newGid) {
            var request = $http.put(BASE_STUDY_URL + studyContext.studyId + '/entries/' + entryId,
                {
                    gid: newGid
                });
            return request.then(successHandler, failureHandler);
        };

		studyEntryService.getStudyEntriesUrl = function() {
			return BASE_STUDY_URL + studyContext.studyId + '/entries';
        };

		studyEntryService.getStudyEntries = function() {
			var request = $http.post(studyEntryService.getStudyEntriesUrl(), {});
			return request.then(successHandler, failureHandler);
		};

		studyEntryService.getEntryTableColumns = function() {
			var request = $http.get(BASE_STUDY_URL + studyContext.studyId + '/entries/table/columns');
			return request.then(successHandler, failureHandler);
		};

		studyEntryService.getEntryTypes = function() {
			var request = $http.get(BASE_PROGRAM_URL + '/entry-types');
			return request.then(successHandler, failureHandler);
		};

		studyEntryService.saveStudyEntries = function(entryTypeId, itemIds) {
			var request = $http.put(BASE_STUDY_URL + studyContext.studyId + '/entries/', {
				"entryTypeId": entryTypeId,
				"searchComposite": {
					"itemIds": itemIds,
					"searchRequest": null
				}
			});

			return request.then(successHandler, failureHandler);
        };

		studyEntryService.saveStudyEntriesList = function(listId) {
			var request = $http.post(BASE_STUDY_URL + studyContext.studyId + '/entries/generation?listId=' + listId);
			return request.then(successHandler, failureHandler);
		};

		studyEntryService.deleteEntries = function() {
			var request = $http.delete(BASE_STUDY_URL + studyContext.studyId + '/entries');
			return request.then(successHandler, failureHandler);
		};

		studyEntryService.updateStudyEntriesProperty = function (entryIds, variableId, newValue) {
			var request = $http.put(BASE_STUDY_URL + studyContext.studyId + '/entries/properties', {
				"searchComposite": {
					"itemIds": entryIds,
					"searchRequest": null
				},
				"variableId": variableId,
				"value": newValue
			});
			return request.then(successHandler, failureHandler);
		};

		studyEntryService.getStudyEntriesMetadata = function () {
			var request = $http.get(BASE_STUDY_URL + studyContext.studyId + '/entries/metadata');
			return request.then(successHandler, failureHandler);
		};

        return studyEntryService;

    }
    ]);


})();

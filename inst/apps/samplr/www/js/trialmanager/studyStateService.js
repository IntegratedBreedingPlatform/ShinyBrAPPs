(function() {
    'use strict';
	var app = angular.module('studyState', []);
	app.service('studyStateService', [
        function() {
            var service = {
                state: {
                    // hasUnsavedData becomes true when adding/deleting environments and variables in a study.
                    hasUnsavedData: false,
                    hasGeneratedDesign: false,
                    hasListOrSubObs: false,
                    hasMeansDataset: false
                },

                updateOccurred: function () {
                    service.state.hasUnsavedData = true;
                },

                resetState: function () {
                    service.state.hasUnsavedData = false;
                },

                hasUnsavedData: function() {
                    return service.state.hasUnsavedData;
                },

                hasGeneratedDesign: function(){
                    return service.state.hasGeneratedDesign
                },

                updateGeneratedDesign: function (designStatus) {
                    service.state.hasGeneratedDesign = designStatus;
                },

                hasListOrSubObs: function () {
                    return service.state.hasListOrSubObs;
                },

                updateHasListsOrSubObs: function (status) {
                    return service.state.hasListOrSubObs = status;
                },

                hasMeansDataset: function () {
                    return service.state.hasMeansDataset;
                },

                updateHasMeansDataset: function (status) {
                    return service.state.hasMeansDataset = status;
                }
            };
            return service;
        }
    ]);
})();

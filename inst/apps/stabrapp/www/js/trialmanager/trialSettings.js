/*global angular*/

(function() {
	'use strict';

	var manageTrialApp = angular.module('manageTrialApp');

	manageTrialApp.controller('TrialSettingsCtrl', ['$scope', 'TrialManagerDataService', '_', '$filter', 'studyStateService', function ($scope, TrialManagerDataService, _, $filter, studyStateService) {

		$scope.data = TrialManagerDataService.currentData.trialSettings;
		$scope.addVariable = true;

		$scope.managementDetails = TrialManagerDataService.settings.trialSettings;

		$scope.managementDetailsOptions = {
			selectAll: false
		};

		$scope.doSelectAll = function(variables, options) {

			var filteredVariables = $filter('removeHiddenAndDeletablesVariableFilter')(variables.keys(), variables.vals());

			_.each(filteredVariables, function(cvTermID) {
                variables.val(cvTermID).isChecked = options.selectAll;
			});

		};

		$scope.removeSettings = function (variableType, variables, options) {
			TrialManagerDataService.removeSettings(variableType, variables).then(function (data) {
				_(data).each(function (ids) {
					delete $scope.data.userInput[ids];
				});

				options.selectAll = false;
				studyStateService.updateOccurred();
			});
		};

		$scope.onLabelChange = function() {
			studyStateService.updateOccurred();
		};

		$scope.$on('variableAdded', function () {
			studyStateService.updateOccurred();
		});

	}]);

})();

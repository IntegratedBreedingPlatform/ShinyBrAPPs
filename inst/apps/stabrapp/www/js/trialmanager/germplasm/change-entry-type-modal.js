(function () {
	'use strict';

	const module = angular.module('manageTrialApp');

	module.controller('changeStudyEntryTypeCtrl', ['$scope', '$rootScope', '$uibModalInstance', 'studyEntryService', 'entryIds', 'currentValue',
		function ($scope, $rootScope, $uibModalInstance, studyEntryService, entryIds, currentValue) {

			$scope.selected = {};
			$scope.entryTypes = [];
			$scope.init = function () {
				studyEntryService.getEntryTypes().then(function (entryTypes) {
					buildEntryTypes(entryTypes);
				})
			};

			$scope.cancel = function () {
				$uibModalInstance.dismiss();
			};


			$scope.editEntryType = function () {
				studyEntryService.updateStudyEntriesProperty(entryIds, 8255, $scope.selected.entryType.id).then(function (response) {
					showSuccessfulMessage('',$.germplasmMessages.editEntryTypeSuccess);
					$uibModalInstance.close();
					$rootScope.$emit("reloadStudyEntryTableData", {});
				}, function(errResponse) {
					$uibModalInstance.close();
					showErrorMessage($.fieldbookMessages.errorServerError,  errResponse.errors[0].message);
				});
			};

			function buildEntryTypes(entryTypes) {
				entryTypes.forEach(function (entryType) {
					$scope.entryTypes.push(entryType);
					if(currentValue && entryType.id === parseInt(currentValue)) {
						$scope.selected.entryType = entryType;
					}
				});
				if(!currentValue) {
					$scope.selected.entryType = $scope.entryTypes[0];
				}
			}

			$scope.init();
		}
	]);

})();

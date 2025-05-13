(function () {
	'use strict';

	const module = angular.module('manageTrialApp');

	module.controller('InventoryDetailsCtrl', ['$scope', '$q', 'studyContext', '$uibModalInstance', 'gid',
		function ($scope, $q, studyContext, $uibModalInstance, gid) {

			$scope.url = '/ibpworkbench/controller/jhipster#/inventory-details?restartApplication' +
				'&cropName=' + studyContext.cropName +
				'&programUUID=' + studyContext.programId +
				'&gid=' + gid;

			window.closeModal = function() {
				$uibModalInstance.close(null);
			};

			$scope.cancel = function() {
				$uibModalInstance.close(null);
			};

		}]);

})();

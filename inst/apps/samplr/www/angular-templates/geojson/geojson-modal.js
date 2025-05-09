(function () {
	'use strict';

	const module = angular.module('manageTrialApp');

	module.controller('GeoJSONModalCtrl', ['$scope', 'studyContext', '$uibModalInstance', 'helpLinkService',
		function ($scope, studyContext, $uibModalInstance, helpLinkService) {
			$scope.geoJSONEditorUrl = '/ibpworkbench/controller/brapi-fieldmap?'
				+ 'instanceId=' + $scope.$resolve.instanceId
				+ '&cropName=' + studyContext.cropName;

			helpLinkService.helpLink($scope.$resolve.helpModule).then(function (url){
				 $scope.helpToolUrl = url;
			});

			if ($scope.$resolve.isViewGeoJSON) {
				$scope.geoJSONEditorUrl += '&hasLayout=true';
			}

			$scope.cancel = function () {
				$uibModalInstance.dismiss();
			};
		}
	]);
})();

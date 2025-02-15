'use strict';

describe('GermplasmSelectorCtrl:', function () {
	var $controller,
		$rootScope,
		$q,
		controller,
		scope;

	// Mocks
	var studyContextMock = {
			studyId: 1,
			cropName: 'maize',
			programId : 'abc-123'
		},
		uibModalInstanceMock = {
			close: jasmine.createSpy('close'),
			dismiss: jasmine.createSpy('dismiss'),
			result: {
				then: jasmine.createSpy('then')
			}
		},
		gids = [11, 13, 15];

	beforeEach(function () {
		module('manageTrialApp');

		module(function ($provide) {
			$provide.value("studyContext", studyContextMock);
			$provide.value("$uibModalInstance", uibModalInstanceMock);
			$provide.value("selectMultiple", false);
		});

		inject(function ($injector) {
			scope = $injector.get('$rootScope').$new();
			var $controller = $injector.get('$controller');
			$q = $injector.get('$q');
			uibModalInstanceMock = $injector.get('$uibModalInstance');

			controller = $controller('GermplasmSelectorCtrl', {
				$scope: scope,
				$rootScope: $rootScope,
				$q : $q,
				studyContext: studyContextMock,
				$uibModalInstance: uibModalInstanceMock
			});

		});
	});

	describe('Select Germplasm Modal', function () {
		it('Germplasm selector URL should be correct', function () {
			expect(scope.url).toBe('/ibpworkbench/controller/jhipster#/germplasm-selector?restartApplication' +
				'&cropName=' + studyContextMock.cropName +
				'&programUUID=' + studyContextMock.programId +
				'&selectMultiple=false');
		});

		it('should return selected GIDS upon closing', function () {
			window.onGidsSelected(gids);
			expect(uibModalInstanceMock.close).toHaveBeenCalledWith(gids);
		});

		it('should close Select Germplasm modal', function () {
			window.closeModal();
			expect(uibModalInstanceMock.close).toHaveBeenCalledWith(null);
		});
	})

});

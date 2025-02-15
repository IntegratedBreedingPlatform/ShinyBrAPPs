'use strict';

describe('Study State Service', function() {

    var studyStateService;

	beforeEach(function() {
		module('studyState');
	});


	beforeEach(inject(function ($injector) {
		studyStateService = $injector.get('studyStateService');
	}));

    describe('updateOccurred', function() {

        it('should update hasUnsavedData to true', function() {

            studyStateService.updateOccurred();

            expect(studyStateService.hasUnsavedData()).toEqual(true);

        });

    });

    describe('resetState', function() {

        it('should reset hasUnsavedData value to false', function() {

            studyStateService.updateOccurred();
            studyStateService.resetState();

            expect(studyStateService.hasUnsavedData()).toEqual(false);

        });

    });

});

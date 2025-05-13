/*global angular, expect, inject, spyOn*/
'use strict';
describe('Study Settings', function () {
    var controller;
    var scope;
    var studyStateService;
    var directiveElement = {
        changefunction: function () {
            studyStateService.updateOccurred();
        }
    }
    beforeEach(function () {
        module('studyState');
    });
    beforeEach(inject(function (_$controller_, $rootScope, _$q_, $injector) {
            studyStateService = $injector.get('studyStateService');
            scope =  $rootScope.$new();
        }));

    describe('On Change of settings', function () {
        it('Should have unsaved Data', function () {
            //Mock of onchangefunction
            directiveElement.changefunction();
            expect(studyStateService.hasUnsavedData()).toEqual(true);
        })
    });


});
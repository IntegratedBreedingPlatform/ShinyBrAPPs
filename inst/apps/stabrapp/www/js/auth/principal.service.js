(function () {
	'use strict';

	const module = angular.module('auth');

	module.factory('PrincipalService', ['AccountService', '$q',
		function (AccountService, $q) {

			var service = {
				userIdentity: null
			};

			const deferred = $q.defer();

			service.principal = function () {
				if (service.userIdentity) {
					return $q.resolve(service.userIdentity);
				}
				return deferred.promise;
			}

			AccountService.get().then((account) => {
				service.userIdentity = account;
				deferred.resolve(service.userIdentity);
			});

			return service;
		}
	]);
})();

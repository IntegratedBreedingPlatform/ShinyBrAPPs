(function () {
	'use strict';

	const module = angular.module('auth');

	module.factory('HasAnyAuthorityService', ['PrincipalService',
		function (PrincipalService) {

			var service = {};

			/**
			 * e.g hasAuthorityMap['ADMIN'] = true
			 * @type {{string: boolean}}
			 */
			var hasAuthorityMap = {};

			service.hasAnyAuthority = function (value) {
				const authorities = typeof value === 'string' ? [value] : value;
				return authorities.some((authority) => hasAuthorityMap[authority]);
			}

			PrincipalService.principal().then((account) => {
				if (account && account.authorities && account.authorities.length) {
					account.authorities.forEach((authority) => {
						hasAuthorityMap[authority] = true;
					});
				}
			});

			return service;
		}
	]);
})();

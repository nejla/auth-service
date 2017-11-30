function initializeAuthService(app) {
    app.factory('AuthenticationResponseObserver', ['$q', '$window', function ($q, $window) {
        return {
            responseError: function (response) {
                if (response.status === 403) {
                    $window.location = '/auth.html';
                };

                return $q.reject(response);
            }
        };
    }]);

    app.config(['$httpProvider', function ($httpProvider) {
        $httpProvider.interceptors.push('AuthenticationResponseObserver');
    }]);
}

function logout($http) {
    $http.post('/api/logout').then(function (result) {
        window.location.reload(true);
    });
}

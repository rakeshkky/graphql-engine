#!/usrbin/env python3

import pytest
from validate import check_query_f
from super_classes import DefaultTestSelectQueries

@pytest.mark.skipif(not pytest.config.getoption("--test-whitelisted-queries"),
                    reason="flag --test-whitelisted-queries is not set. Cannot runt tests for whitelisted queries")
@pytest.mark.parametrize("transport", ['http','websocket'])
class TestWhitelistedQueries(DefaultTestSelectQueries):

    def test_query_user(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_user.yaml', transport)

    def test_query_user_by_pk(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_user_by_pk.yaml', transport)

    def test_query_user_with_typename(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_user_with_typename.yaml', transport)

    def test_query_non_whitelisted(self, hge_ctx, transport):
        check_query_f(hge_ctx, self.dir() + '/query_non_whitelisted.yaml', transport)

    @classmethod
    def dir(cls):
        return 'queries/graphql_query/whitelisted'

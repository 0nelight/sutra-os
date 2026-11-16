from __future__ import annotations


class SutraError(Exception):
    pass


class AuthError(SutraError):
    pass


class NotFound(SutraError):
    pass


class QueryError(SutraError):
    pass


class ServerError(SutraError):

    def __init__(self, status: int, body):
        self.status = status
        self.body = body
        self.reason = body.get("error") if isinstance(body, dict) else None
        super().__init__(f"HTTP {status}: {self.reason or repr(body)}")


class ResultTruncated(SutraError):
    pass

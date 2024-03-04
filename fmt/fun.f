*     FUN(x, y) = | sin(x) / cos(y) | (true)
*     FUN(x, 90 + 180 * K) -> none (false)
      LOGICAL*1 FUNCTION FUN(X, Y)
        DOUBLE PRECISION PI, X, Y, Z, XR, YR
        PARAMETER(PI = 4.D0 * DATAN(1.D0))
        COMMON /f/ Z

        IF (DMOD(DABS(Y), 180.D0) .EQ. 90.D0) THEN
          FUN = .FALSE.
        ELSE
          XR = X / 180.D0 * PI
          YR = Y / 180.D0 * PI
          Z = DABS(DSIN(XR)/DCOS(YR))
          FUN = .TRUE.
        END IF
      END


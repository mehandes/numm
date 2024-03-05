      LOGICAL*1 FUNCTION FUN(X, Y)
        DOUBLE PRECISION PI, X, Y, Z, XR, YR
        PARAMETER(PI = 4.D0 * DATAN(1.D0))
        COMMON /z/ Z

        IF (DMOD(DABS(Y), 180.D0) .EQ. 90.D0) THEN
          FUN = .FALSE.
        ELSE
          IF (DMOD(DABS(X), 180.D0) .EQ. 0.D0) THEN
            Z = 0.D0
          ELSE
            XR = X / 180.D0 * PI
            YR = Y / 180.D0 * PI
            Z = DABS(DSIN(XR)/DCOS(YR))
          END IF
          FUN = .TRUE.
        END IF
      END


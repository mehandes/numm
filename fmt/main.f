      PROGRAM MAIN
        LOGICAL CONFIGURE
        REAL XB, XE, XS, XM
        REAL YB, YE, YS, YM
        REAL PI

        COMMON /x/ XB, XE, XS, XM
        COMMON /y/ YB, YE, YS, YM
        COMMON /const/ PI

        PI = 3.1415926535897931

        IF (CONFIGURE()) THEN
          CALL DRAW()
        END IF
      END

      LOGICAL FUNCTION CONFIGURE()
        LOGICAL CHECK_STEP, CHECK_RANGE
        REAL XB, XE, XS, XM
        REAL YB, YE, YS, YM

        COMMON /x/ XB, XE, XS, XM
        COMMON /y/ YB, YE, YS, YM

        OPEN(1, FILE='in.txt')
        READ(1, *) XB, XE, XS, YB, YE, YS
        CLOSE(1)

        XM = (MIN(ABS(XB), ABS(XE)) - XS) * 0.0001
        YM = (MIN(ABS(YB), ABS(YE)) - YS) * 0.0001
        CONFIGURE = .TRUE.

        IF (CHECK_STEP(XB, XE, XS)) THEN
          PRINT *, 'Error: invisible X step.'
          CONFIGURE = .FALSE.
        ELSE IF (CHECK_STEP(YB, YE, YS)) THEN
          PRINT *, 'Error: invisible Y step.'
          CONFIGURE = .FALSE.
        ELSE IF (CHECK_RANGE(XB, XE, XS)) THEN
          PRINT *, 'Error: incorrect X range.'
          CONFIGURE = .FALSE.
        ELSE IF (CHECK_RANGE(YB, YE, YS)) THEN
          PRINT *, 'Error: incorrect Y range.'
          CONFIGURE = .FALSE.
        END IF
      END

      LOGICAL FUNCTION CHECK_STEP(B, E, S)
        REAL B, E, S
        CHECK_STEP = S .LE. (MAX(ABS(B), ABS(E)) - S) * 0.0001
      END

      LOGICAL FUNCTION CHECK_RANGE(B, E, S)
        REAL B, E, S
        CHECK_RANGE = B .GT. E .OR. S .LT. 0.0
      END

      SUBROUTINE DRAW()
        REAL XB, XE, XS, XM, X
        REAL YB, YE, YS, YM, Y

        COMMON /x/ XB, XE, XS, XM
        COMMON /y/ YB, YE, YS, YM

10      FORMAT(5x, '*', 5x, ' | '$)
20      FORMAT(E11.4, ' | '$)
30      FORMAT(E11.4, ' | ')

        OPEN(2, FILE='out.txt')

*       Draw Xs
        X = XB
        WRITE(2, 10)
1       IF (X .LT. XE) THEN
          CALL CHECK_ZERO(X, XM)
          WRITE(2, 20) X
          X = X + XS
          GO TO 1
        END IF
        WRITE(2, 30) XE

        CALL DRAW_BORDER()

*       Draw values without YE
        Y = YB 
2       IF (Y .LT. YE) THEN
          CALL CHECK_ZERO(Y, YM)
          WRITE(2, 20) Y

          X = XB
3         IF (X .LT. XE) THEN
            CALL CHECK_ZERO(X, XM)
            CALL VALUE(X, Y)
            
            X = X + XS
            GO TO 3
          END IF

          CALL VALUE(XE, Y)
          WRITE(2, '()')
          CALL DRAW_BORDER()

          Y = Y + YS
          GO TO 2
        END IF

*       Draw values for YE
        WRITE(2, 20) YE
        X = XB
4       IF (X .LT. XE) THEN
          CALL CHECK_ZERO(X, XM)
          CALL VALUE(X, YE)
          
          X = X + XS
          GO TO 4
        END IF

        CALL VALUE(XE, YE)
        WRITE(2, '()')

        CLOSE(2)
      END

      SUBROUTINE DRAW_BORDER()
        REAL XB, XE, XS, XM, X
        REAL YB, YE, YS, YM, Y

        COMMON /x/ XB, XE, XS, XM
        COMMON /y/ YB, YE, YS, YM

10      FORMAT(11('-'), 3x, ''$)
20      FORMAT(11('-'), 3x)
        WRITE(2, 10)

        X = XB
1       IF (X .LT. XE) THEN
          WRITE(2, 10)
          X = X + XS
          GO TO 1
        END IF

        WRITE(2, 20)
      END

      SUBROUTINE CHECK_ZERO(X, XM)
        REAL X, XM
        
        IF (ABS(X) .LE. XM) THEN
          X = 0.0
        END IF
      END

      SUBROUTINE VALUE(X, Y)
        LOGICAL EQ

        REAL X, Y, PI
        REAL XB, XE, XS, XM
        REAL YB, YE, YS, YM
        REAL XR, YR

        COMMON /x/ XB, XE, XS, XM
        COMMON /y/ YB, YE, YS, YM
        COMMON /const/ PI

10      FORMAT(5x, '*', 5x, ' | '$)
20      FORMAT(E11.4, ' | '$)

        IF (EQ(MOD(ABS(Y), 180.0), 90.0, YM)) THEN
          WRITE(2, 10)
        ELSE
          XR = X / 180.0 * PI
          YR = Y / 180.0 * PI
          WRITE(2, 20) ABS(SIN(XR) / COS(YR))
        END IF
      END

      LOGICAL FUNCTION EQ(A, B, M)
        REAL A, B, M
        EQ = ABS(A - B) .LE. M 
      END



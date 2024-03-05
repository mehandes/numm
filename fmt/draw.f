      SUBROUTINE DRAW_TABLE
        DOUBLE PRECISION XB, XE, XS, X
        DOUBLE PRECISION YB, YE, YS, Y
        DOUBLE PRECISION Z

        COMMON /x/ XB, XE, XS
        COMMON /y/ YB, YE, YS
        COMMON /z/ Z

        CALL DRAW_X(XB, XE, XS)
        CALL DRAW_DELIM(XB, XE, XS)

        Y = YB
10      IF (Y .LT. YE) THEN
          CALL DRAW_LINE(Y, XB, XE, XS)
          CALL DRAW_DELIM(XB, XE, XS)
          Y = Y + YS
          GOTO  10
        END IF

        CALL DRAW_LINE(YE, XB, XE, XS)
      END

      SUBROUTINE DRAW_X(XB, XE, XS)
        DOUBLE PRECISION XB, XE, XS, X

        WRITE(1, '("      *      |")', ADVANCE="no")

        X = XB
10      IF (X .LT. XE) THEN
          WRITE(1, '(E12.4 " |")', ADVANCE="no") X
          X = X + XS
          GOTO 10
        END IF
        WRITE(1, '(E12.4)') XE
      END

      SUBROUTINE DRAW_LINE(Y, XB, XE, XS)
        LOGICAL*1 FUN
        DOUBLE PRECISION Y, XB, XE, XS, X, Z
        COMMON /z/ Z

1       FORMAT(E12.4 ' |')
        WRITE(1, 1, ADVANCE="no") Y

        X = XB
10      IF (X .LT. XE) THEN
          IF (FUN(X, Y)) THEN
            WRITE(1, 1, ADVANCE="no") Z
          ELSE
            WRITE(1, '("      *      |")', ADVANCE="no")
          END IF

          X = X + XS
          GOTO 10
        END IF

        IF (FUN(XE, Y)) THEN
          WRITE(1, '(E12.4)') Z
        ELSE
          WRITE(1, '("     *     ")')
        END IF
      END

      SUBROUTINE DRAW_DELIM(XB, XE, XS)
        DOUBLE PRECISION XB, XE, XS

        X = XB
        WRITE(1, '( " -----------  " )', ADVANCE="no")
10      IF (X .LT. XE) THEN
          WRITE(1, '( " -----------  " )', ADVANCE="no")
          X = X + XS
          GOTO 10
        END IF
        WRITE(1, '( " -----------  " )')
      END


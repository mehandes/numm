      PROGRAM MAIN
        CALL CONFIGURE()
        CALL TABLE()
      END

      SUBROUTINE CONFIGURE
        DOUBLE PRECISION XSTART, XEND, XSTEP, X
        DOUBLE PRECISION YSTART, YEND, YSTEP, Y

        COMMON /x/ XSTART, XEND, XSTEP
        COMMON /y/ YSTART, YEND, YSTEP

        READ(*, *) XSTART, XEND, XSTEP
        READ(*, *) YSTART, YEND, YSTEP
      END

      SUBROUTINE TABLE
        LOGICAL*1 FUN
        DOUBLE PRECISION XSTART, XEND, XSTEP, X
        DOUBLE PRECISION YSTART, YEND, YSTEP, Y

        COMMON /x/ XSTART, XEND, XSTEP
        COMMON /y/ YSTART, YEND, YSTEP
        COMMON /f/ Z

1       FORMAT (E12.4 ' |')
2       FORMAT ('      -      |')
3       FORMAT(E12.4)
4       FORMAT('      -      ')
5       FORMAT(' ------------ ')

        WRITE(*, 2, ADVANCE="no")
        X = XSTART
6       IF (X .LT. XEND) THEN
          WRITE(*, 1, ADVANCE="no") X
          X = X + XSTEP
          GOTO 6
        END IF
        WRITE(*, 3) XEND

        Y = YSTART
7       IF (Y .LE. YEND) THEN
          WRITE(*, 1, ADVANCE="no") Y

          X = XSTART
8         IF (X .LT. XEND) THEN
            IF (FUN(X, Y)) THEN
              WRITE(*, 1, ADVANCE="no") Z
            ELSE 
              WRITE(*, 2, ADVANCE="no")
            END IF

            X = X + XSTEP
            GOTO 8
          END IF

          IF (FUN(XEND, Y)) THEN
            WRITE(*, 3) Z
          ELSE
            WRITE(*, 4)
          END IF

          X = XSTART
9         IF (X .LT. XEND) THEN
            WRITE(*, 5, ADVANCE="no")
            X = X + XSTEP
            GOTO 9
          END IF
          WRITE(*, 5)
         
          Y = Y + YSTEP
          GOTO 7
        END IF
      END

      SUBROUTINE WRITE_LINE(Y, XB, XE, XS)
        DOUBLE PRECISION Y, XB, XE, XS
        
        FORMAT()
      END

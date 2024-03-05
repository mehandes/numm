      PROGRAM MAIN
        OPEN(1, FILE="out.txt")
        OPEN(2, FILE="in.txt")

        CALL CONFIGURE()
        CALL DRAW_TABLE()

        CLOSE(1)
        CLOSE(2)
      END

      SUBROUTINE CONFIGURE
        DOUBLE PRECISION XB, XE, XS
        DOUBLE PRECISION YB, YE, YS

        COMMON /x/ XB, XE, XS
        COMMON /y/ YB, YE, YS

        READ(2, *) XB, XE, XS
        READ(2, *) YB, YE, YS
      END

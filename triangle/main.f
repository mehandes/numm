      PROGRAM MAIN
        REAL A, B, C, AREA, MIN_ANGLE, COS_MIN_ANGLE
        LOGICAL VALID, READ_TRIANGLE
        INTEGER COMMAND, MENU
        COMMON A, B, C

        VALID = .FALSE.
6       COMMAND = MENU(VALID)
        PRINT *, ACHAR(27)//"[2J"
        GO TO (1, 2, 3, 4, 5), COMMAND
1       VALID = READ_TRIANGLE()
        GO TO 6
2       CALL EXIT(0)
3       PRINT *, 'Area: ', AREA(A, B, C)
        GO TO 6
4       PRINT *, 'Smallest angle: ', MIN_ANGLE(A, B, C)
        GO TO 6       
5       PRINT *, 'Cosine of smallest angle: ', COS_MIN_ANGLE(A, B, C)
        GO TO 6
      END

*     Show menu and read command.
*     @returns - command
      INTEGER FUNCTION MENU(VALID)
        INTEGER COMMAND
        LOGICAL VALID, VL, IL

7       CALL SHOW_MENU(VALID)
        READ *, COMMAND

        VL = VALID .AND. (COMMAND .GT. 5)
        IL = (.NOT. VALID) .AND. (COMMAND .GT. 2)

        IF ((COMMAND .LT. 1) .OR. VL .OR. IL) THEN
          PRINT *, ACHAR(27)//"[2J"
          PRINT *, 'Unsupported command.'
          GO TO 7
        END IF

        MENU = COMMAND
      END

      SUBROUTINE SHOW_MENU(VALID)
        LOGICAL VALID

        PRINT *, 'Select one of the following commands:'
        PRINT *, '1. New triangle'
        PRINT *, '2. Exit'

        IF (VALID) THEN
          PRINT *, '3. Area'
          PRINT *, '4. Smallest angle'
          PRINT *, '5. Cosine of the smallest angle'
        END IF
      END

*     Read the coordinates of the vertex of the triangle and calculate the sides from them.
*     @result - A, B, C - triangle sides.
*     @returns Wether triangle valid or not.
      LOGICAL FUNCTION READ_TRIANGLE()
        REAL X1, Y1, X2, Y2, X3, Y3, A, B, C
        LOGICAL VALIDATE
        COMMON A, B, C

        PRINT *, 'First point (e.g. "0 0"):'
        READ *, X1, Y1
        PRINT *, 'Second point (e.g. "4 0"):'
        READ *, X2, Y2
        PRINT *, 'Third point (e.g. "0 3"):'
        READ *, X3, Y3

        IF (VALIDATE(X1, Y1, X2, Y2, X3, Y3)) THEN
          CALL CONVERT(X1, Y1, X2, Y2, X3, Y3)
          READ_TRIANGLE = .TRUE.
          PRINT *, 'Triangle submitted.'
        ELSE
          READ_TRIANGLE = .FALSE.
          PRINT *, 'Triangle rejected.'
        ENDIF
      END


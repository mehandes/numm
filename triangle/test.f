      PROGRAM TEST_ALL
        CALL TEST_VALIDATE()
        CALL TEST_CONVERT()
        CALL TEST_MIN_SIDE()
        CALL TEST_MIN_ANGLE()
        CALL TEST_COS_MIN_ANGLE()
      END

      SUBROUTINE TEST_VALIDATE()
        LOGICAL VALIDATE

        PRINT *, 'Testing VALIDATE()...'
        CALL TEST(1, VALIDATE(0.0, 0.0, 0.0, 4.0, 3.0, 0.0)) ! Right
        CALL TEST(2, VALIDATE(-3.0, 0.0, 0.0, 3.0, 3.0, 0.0)) ! Isosceles
        CALL TEST(3, VALIDATE(0.0, 0.0, 2.0, 0.0, 1.0, SQRT(3.0))) ! Equilateral
        CALL TEST(4, VALIDATE(-3.0, 0.0, 0.0, 3.0, 2.0, 0.0)) ! Acute
        CALL TEST(5, VALIDATE(-2.0, 3.0, 0.0, 3.0, 4.0, 0.0)) ! Obtuse
        CALL TEST(6, VALIDATE(-2.0, 0.0, 0.0, 3.0, 2.0, -2.0)) ! Negative coords
        CALL TEST(7, .NOT. VALIDATE(0.0, 0.0, 2.0, 2.0, 4.0, 4.0)) ! Degenerate
        CALL TEST(8, .NOT. VALIDATE(-2.0, -2.0, -1.0, -1.0, 2.0, 2.0)) ! Degenerate negative coords
      END

      SUBROUTINE TEST_CONVERT()
        REAL A, B, C
        LOGICAL L
        COMMON A, B, C

        PRINT *, 'Testing CONVERT()...'
        CALL CONVERT(0.0, 0.0, 0.0, 4.0, 3.0, 0.0)
        CALL TEST(1, A .EQ. 4 .AND. B .EQ. 5 .AND. C .EQ. 3) ! Right

        CALL CONVERT(-3.0, 0.0, 0.0, 3.0, 3.0, 0.0)
        L = A .EQ. SQRT(18.0)
        CALL TEST(2, L .AND. B .EQ. SQRT(18.0) .AND. C .EQ. 6) ! Isosceles

        CALL CONVERT(0.0, 0.0, 2.0, 0.0, 1.0, SQRT(3.0))
        CALL TEST(3, A .EQ. 2 .AND. B .EQ. 2 .AND. C .EQ. 2) ! Equilateral

        CALL CONVERT(-3.0, 0.0, 0.0, 3.0, 2.0, 0.0)
        L = A .EQ. SQRT(18.0)
        CALL TEST(4, L .AND. B .EQ. SQRT(13.0) .AND. C .EQ. 5) ! Acute

        CALL CONVERT(0.0, 0.0, -2.0, 3.0, 4.0, 0.0)
        L = A .EQ. SQRT(13.0)
        CALL TEST(5, L .AND. B .EQ. SQRT(45.0) .AND. C .EQ. 4) ! Obtuse

        CALL CONVERT(-2.0, 0.0, 0.0, 3.0, 2.0, -2.0)
        L = A .EQ. SQRT(13.0)
        CALL TEST(6, L .AND. B .EQ. SQRT(29.0) .AND. C .EQ. SQRT(20.0)) ! Negative coords
      END

      SUBROUTINE TEST_AREA()
        LOGICAL EQUALS
        REAL AREA

        PRINT *, 'Testing AREA()...'
        CALL TEST(1, EQUALS(AREA(4.0, 3.0, 5.0), 6.0)) ! Right
        CALL TEST(2, EQUALS(AREA(6.0, SQRT(18.0), SQRT(18.0)), 9.0)) ! Isosceles
        CALL TEST(3, EQUALS(AREA(2.0, 2.0, 2.0), SQRT(3.0))) ! Equilateral
        CALL TEST(4, EQUALS(AREA(5.0, SQRT(18.0), SQRT(13.0)), 7.5)) ! Acute
        CALL TEST(5, EQUALS(AREA(4.0, SQRT(13.0), SQRT(45.0)), 6.0)) ! Obtuse
      END

      SUBROUTINE TEST_MIN_SIDE()
        LOGICAL EQUALS
        REAL MIN_SIDE, R

        PRINT *, 'Testing MIN_SIDE()...'
        CALL TEST(1, EQUALS(MIN_SIDE(4.0, 3.0, 5.0), 3.0)) ! Right

        R = MIN_SIDE(6.0, SQRT(18.0), SQRT(18.0))
        CALL TEST(2, EQUALS(R, SQRT(18.0))) ! Isosceles
      
        CALL TEST(3, EQUALS(MIN_SIDE(2.0, 2.0, 2.0), 2.0)) ! Equilateral

        R = MIN_SIDE(5.0, SQRT(18.0), SQRT(13.0))
        CALL TEST(4, EQUALS(R, SQRT(13.0))) ! Acute

        R = MIN_SIDE(4.0, SQRT(13.0), SQRT(45.0))
        CALL TEST(5, EQUALS(R, SQRT(13.0))) ! Obtuse
      END

      SUBROUTINE TEST_MIN_ANGLE()
        LOGICAL EQUALS
        REAL MIN_ANGLE, R, PI
        PARAMETER (PI = 3.1415927)

        PRINT *, 'Testing MIN_ANGLE()...'
        
        R = MIN_ANGLE(4.0, 3.0, 5.0)
        CALL TEST(1, EQUALS(R, ATAN(3.0 / 4.0) * 180.0 / PI)) ! Right

        R = MIN_ANGLE(6.0, SQRT(18.0), SQRT(18.0))
        CALL TEST(2, EQUALS(R, 45.0)) ! Isosceles

        R = MIN_ANGLE(2.0, 2.0, 2.0)
        CALL TEST(3, EQUALS(R, 60.0)) ! Equilateral

        R = MIN_ANGLE(5.0, SQRT(18.0), SQRT(13.0))
        CALL TEST(4, EQUALS(R, 45.0)) ! Acute

        R = MIN_ANGLE(4.0, SQRT(13.0), SQRT(45.0))
        CALL TEST(5, EQUALS(R, ATAN(0.5) * 180.0 / PI)) ! Obtuse
      END

      SUBROUTINE TEST_COS_MIN_ANGLE()
        LOGICAL EQUALS
        REAL COS_MIN_ANGLE, R, PI
        PARAMETER (PI = 3.1415927)

        PRINT *, 'Testing COS_MIN_ANGLE()...'
        
        R = COS_MIN_ANGLE(4.0, 3.0, 5.0)
        CALL TEST(1, EQUALS(R, COS(ATAN(3.0 / 4.0)))) ! Right

        R = COS_MIN_ANGLE(6.0, SQRT(18.0), SQRT(18.0))
        CALL TEST(2, EQUALS(R, COS(PI / 4.0))) ! Isosceles

        R = COS_MIN_ANGLE(2.0, 2.0, 2.0)
        CALL TEST(3, EQUALS(R, COS(PI / 3.0))) ! Equilateral

        R = COS_MIN_ANGLE(5.0, SQRT(18.0), SQRT(13.0))
        CALL TEST(4, EQUALS(R, COS(PI / 4.0))) ! Acute

        R = COS_MIN_ANGLE(4.0, SQRT(13.0), SQRT(45.0))
        CALL TEST(5, EQUALS(R, COS(ATAN(0.5)))) ! Obtuse
      END

      SUBROUTINE TEST(N, PASS)
        INTEGER N
        LOGICAL PASS

        IF (PASS) THEN
          PRINT *, N, ' - PASS'
        ELSE
          PRINT *, N, ' - FAIL'
        END IF
      END

      LOGICAL FUNCTION EQUALS(A, B)
        REAL A, B

        EQUALS = ABS(A - B) .LE. 1e-4
      END
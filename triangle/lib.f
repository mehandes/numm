*     Validate given triangle based on its coorinates.
*     @params - triangle coordinates
*     @returns - wether triangle valid or not
      LOGICAL FUNCTION VALIDATE(X1, Y1, X2, Y2, X3, Y3)
        REAL X1, Y1, X2, Y2, X3, Y3

        VALIDATE = (X1 - X2) * (Y3 - Y2) - (Y1 - Y2) * (X3 - X2) .NE. 0
      END

*     Convert three points into triangle sides.
*     @params - triangle coordinates
*     @returns - COMMON( triangle sides )
      SUBROUTINE CONVERT(X1, Y1, X2, Y2, X3, Y3)
        REAL X1, Y1, X2, Y2, X3, Y3, A, B, C
        COMMON A, B, C
 
        A = SQRT((X2 - X1) ** 2 + (Y2 - Y1) ** 2)
        B = SQRT((X3 - X2) ** 2 + (Y3 - Y2) ** 2)
        C = SQRT((X3 - X1) ** 2 + (Y3 - Y1) ** 2)
      END 

*     Calculate the area of a triangle using Heron's formula.
*     @params - triangle sides
*     @returns - calculated area
      REAL FUNCTION AREA(A, B, C)
        REAL A, B, C, P

        P = (A + B + C) / 2
        AREA = SQRT(P * (P - A) * (P - B) * (P - C))
      END

*     Calculate the smallest side of the triangle.
*     @params - triangle sides
*     @returns - smallest side
      REAL FUNCTION MIN_SIDE(A, B, C)
        REAL A, B, C

        MIN_SIDE = MIN(A, MIN(B, C))
      END

*     Calculate the smallest angle of the triangle.
*     @params - triangle sides
*     @returns - smallest angle
      REAL FUNCTION MIN_ANGLE(A, B, C)
        REAL A, B, C, COS_MIN_ANGLE, PI
        PARAMETER (PI = 3.1415927)
        
        MIN_ANGLE = ACOS(COS_MIN_ANGLE(A, B, C)) * 180.0 / PI
      END

*     Calculate the cosine of the smallest angle of the triangle.
*     @params - triangle sides
*     @returns - cosine of the smallest angle
      REAL FUNCTION COS_MIN_ANGLE(A, B, C)
        REAL A, B, C, MS, N, MIN_SIDE

        MS = MIN_SIDE(A, B, C)
        N = A ** 2 + B ** 2 + C ** 2 - 2 * MS ** 2
        COS_MIN_ANGLE = N * MS / (2 * A * B * C)
      END


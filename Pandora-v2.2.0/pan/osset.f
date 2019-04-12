      subroutine OSSET
     $(NO,IM,JM,A)
C
C     Rudolf Loeser, 1981 Oct 27
C---- Output routine.
C     !DASH
      save
C     !DASH
      real*8 A, ZERO
      integer I, IE, IM, IS, IZ, J, JM, KNT, NO
      logical ZA
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  LINER, NAUGHTD, HI, BYE
      intrinsic min
C
C               A(IM,JM)
      dimension A(IM,*)
C     !EJECT
C
      call HI ('OSSET')
C     !BEG
      if(NO.gt.0) then
        IE = 0
  100   continue
          IS  = IE+1
          IE  = min((IE+9),IM)
          KNT = IE-IS+1
C
          call LINER     (2, NO)
          write (NO,101) (I,I=IS,IE)
  101     format(' ',4X,'Ray # ',9I13)
          call LINER     (1, NO)
C
          do 104 J = 1,JM
            call NAUGHTD (A(IS,J), 1, KNT, ZA)
            if(.not.ZA) then
              IZ = IE
  102         continue
                if(A(IZ,J).eq.ZERO) then
                  IZ = IZ-1
                  goto 102
                end if
              continue
              write (NO,103) J,(A(I,J),I=IS,IZ)
  103         format(' ',4X,I5,1X,1P9E13.5)
            end if
  104     continue
C
        if(IE.lt.IM) goto 100
      end if
C     !END
      call BYE ('OSSET')
C
      return
      end

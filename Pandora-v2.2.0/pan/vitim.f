      subroutine VITIM
     $(LU,KODE,K,J,DL,XI,A,PRINT)
C
C     Rudolf Loeser, 2005 Jan 27
C---- Sets up PRD printout sections.
C     (This is version 3 of VITIM.)
C     !DASH
      save
C     !DASH
      real*8 A, DL, XI
      integer IPRDF, J, K, KODE, LU
      logical PRINT
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 99),IPRDF)
C     !DASH
      external BROME, LINER, HI, BYE
C
C               DL(K), A(K), XI(K)
      dimension DL(*), A(*), XI(*)
C
      call HI ('VITIM')
C     !BEG
      call BROME   (IPRDF, DL, K, J, PRINT)
      if(PRINT) then
        call LINER (2, LU)
        if(KODE.eq.1) then
          write (LU,100) J,XI(J),DL(J),A(J)
  100     format(' ',I4,'. frequency',:,11X,'XI =',1PE13.5,9X,'DL =',
     $               E13.5,10X,'A =',E13.5)
        else
          write (LU,100) J
        end if
      end if
C     !END
      call BYE ('VITIM')
C
      return
      end

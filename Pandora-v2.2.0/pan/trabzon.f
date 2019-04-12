      subroutine TRABZON
     $(X,IX,TE,XNE,XNC,XNU,CIJ,CHIJ,NO)
C
C     Rudolf Loeser, 2006 Apr 19
C---- Analyzes collisional excitation.
C     !DASH
      save
C     !DASH
      real*8 CHIJ, CIJ, TE, X, XNC, XNE, XNU, ZERO
      integer IL, ILU, INCEI, IU, IX, LIN, N, NL, NO
C     !COM  or  !DASH
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(225),INCEI)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external ABJECT, LINER, SHIM, INDXIJ, BATUMI, HI, BYE
C
      dimension X(*), IX(*)
C
C               CHIJ(N,NL**2), CIJ(N,NL**2), XNU(NSL), XNC(N), XNE(N),
      dimension CHIJ(N,*),     CIJ(N,*),     XNU(*),   XNC(*), XNE(*),
C
C               TE(N)
     $          TE(*)
C
      call HI ('TRABZON')
C     !BEG
      if(NO.gt.0) then
        call ABJECT       (NO)
        write (NO,100) INCEI
  100   format(' ','Collisional de-excitation at depth #',I4,
     $             ' (when Hydrogen contributes)'//
     $         ' ',14X,'de-excitation coefficient for',8X,
     $             'de-excitation rate due to'/
     $         ' ','transition',2(10X,'electrons',6X,'Hydrogen'))
        call LINER        (1, NO)
C
        LIN = 0
        do 102 IU = 2,NL
          do 101 IL = 1,(IU-1)
            call INDXIJ   (IL, IU, ILU)
            if(CHIJ(INCEI,ILU).ne.ZERO) then
              call BATUMI (X, IX, IU, IL, TE(INCEI), XNE(INCEI),
     $                     XNC(INCEI), XNU(IU), XNU(IL),
     $                     CIJ(INCEI,ILU), CHIJ(INCEI,ILU), NO)
              LIN = LIN+1
              call SHIM   (LIN, 5, NO)
            end if
  101     continue
  102   continue
      end if
C     !END
      call BYE ('TRABZON')
C
      return
      end

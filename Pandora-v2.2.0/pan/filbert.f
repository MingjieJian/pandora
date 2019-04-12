      subroutine FILBERT
     $(LS,L,I,GM,CQS,RK,CK,XND,ARHO,CL1,CL1TA,C1L,C1LTA,DUMP)
C
C     Rudolf Loeser, 1981 Dec 11
C---- Prints details, for PECAN.
C     !DASH
      save
C     !DASH
      real*8 ARHO, C1L, C1LTA, CK, CL1, CL1TA, CQS, GM, RK, XND
      integer I, L, LDINT, LS, LUEO
      logical DMPI, DUMP
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
      equivalence (KZQ( 48),LDINT)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, MINNA, HI, BYE
C     !EJECT
C
      call HI ('FILBERT')
C     !BEG
      call MINNA        (DUMP, I, LDINT, DMPI)
C
      if(DMPI) then
        if(LS.ne.L) then
          LS = L
          call LINER    (1, LUEO)
          write (LUEO,100) L
  100     format(' ','*****',4X,'L=',I3//
     $           ' ',2X,'I',7X,'GM(L)',7X,'QS(L)',7X,'RK(L)',
     $               7X,'CK(L)',7X,'N(L)',1X,'(A*RHO)(L,M)',
     $               6X,'C(L,M)',2X,'C(L,M) INC',6X,'C(M,L)',
     $               2X,'C(M,L) INC')
          call LINER    (1, LUEO)
        end if
C
        write (LUEO,101) I,GM,CQS,RK,CK,XND,ARHO,CL1,CL1TA,C1L,C1LTA
  101   format(' ',I3,1P10E12.4)
      end if
C     !END
      call BYE ('FILBERT')
C
      return
      end

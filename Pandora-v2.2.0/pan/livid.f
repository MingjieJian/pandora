      subroutine LIVID
     $(N,NL,M,BDI,XND,CQL,CQLI,ARHO,CINC,FLM,CIJ,DUMP)
C
C     Rudolf Loeser, 1976 Sep 01
C---- Prints debug data for ALTAR.
C     !DASH
      save
C     !DASH
      real*8 ARHO, BDI, CIJ, CINC, CQL, CQLI, FLM, XND
      integer I, L, LDINT, LUEO, M, ML, N, NL
      logical DMPI, DUMP
      character TIT*3
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
      external INDXIJ, LINER, MINNA, HI, BYE
C
C               CQLI(N,NL), XND(N,NL), ARHO(N,NL), CINC(N,NL), CQL(NL),
      dimension CQLI(N,*),  XND(N,*),  ARHO(N,*),  CINC(N,*),  CQL(*),
C
C               CIJ(N,NL**2), FLM(N,NL), BDI(N,NL)
     $          CIJ(N,*),     FLM(N,*),  BDI(N,*)
C     !EJECT
C
      call HI ('LIVID')
C     !BEG
      if(DUMP) then
        do 103 L = 1,NL
          call INDXIJ    (M, L, ML)
          if(L.ne.M) then
C
            if(L.lt.M) then
              TIT = 'M,L'
            else
              TIT = 'L,M'
            end if
            call LINER   (2, LUEO)
            write (LUEO,100) L,CQL(L),TIT
  100       format(' ','*****',4X,'L=',I3,5X,'Q(L)=',F9.6/
     $             ' ',40X,'Term added'/
     $             ' ',2X,'I',4X,'N(L)',7X,'BD(L)',4X,'(A*RHO)(',
     $                 A3,')',1X,'to  C(M,L)',4X,'C(M,L)',6X,
     $                 'F(L,M)',6X,'Q(L,I)')
            call LINER   (1, LUEO)
C
            do 102 I = 1,N
              call MINNA (DUMP, I, LDINT, DMPI)
              if(DMPI) then
                write (LUEO,101) I,XND(I,L),BDI(I,L),ARHO(I,L),
     $                           CINC(I,L),CIJ(I,ML),FLM(I,L),CQLI(I,L)
  101           format(' ',I3,1P7E12.4)
              end if
  102       continue
C
          end if
  103   continue
      end if
C     !END
      call BYE ('LIVID')
C
      return
      end

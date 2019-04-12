      subroutine FLAME
     $(N,M,CK,RS,SUMS,EP1,SUMU,EP2,KDGV,GVL,DUMP)
C
C     Rudolf Loeser, 1981 Dec 11
C---- Prints details, for ALTAR.
C     !DASH
      save
C     !DASH
      real*8 CK, EP1, EP2, GVL, RS, SUMS, SUMU
      integer I, KDGV, LDINT, LUEO, M, N
      logical DMPI, DUMP
      character BLANK*1, FLD*12
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
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, MINNA, HI, BYE
C
C               CK(N,NL), SUMS(N), EP1(N), EP2(N), SUMU(N), GVL(N,NL),
      dimension CK(N,*),  SUMS(*), EP1(*), EP2(*), SUMU(*), GVL(N,*),
C
C               RS(N)
     $          RS(*)
C     !EJECT
C
      call HI ('FLAME')
C     !BEG
      if(DUMP) then
        call LINER   (5, LUEO)
C
        FLD = BLANK
        if(KDGV.eq.1) then
          FLD = '      GVL(M)'
        end if
C
        write (LUEO,100) FLD
  100   format(' ',2X,'I',7X,'CK(M)',10X,'RS',5X,'EP1 sum',9X,'EP1',
     $             5X,'EP2 sum',9X,'EP2',A12)
        call LINER   (1, LUEO)
C
        FLD = BLANK
        do 103 I = 1,N
          call MINNA (DUMP, I, LDINT, DMPI)
          if(DMPI) then
C
            if(KDGV.eq.1) then
              write (FLD,101) GVL(I,M)
  101         format(1PE12.4)
            end if
C
            write (LUEO,102) I,CK(I,M),RS(I),SUMS(I),EP1(I),SUMU(I),
     $                       EP2(I),FLD
  102       format(' ',I3,1P6E12.4,A12)
          end if
  103   continue
C
      end if
C     !END
      call BYE ('FLAME')
C
      return
      end

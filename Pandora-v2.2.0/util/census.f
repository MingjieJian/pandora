      program CENSUS
C     Rudolf Loeser, 1983 Jun 07
C---- Determination of abundance ratios - (83 Apr 29).
C     Adapted for Alpha - 1995 Oct 26
C
C     1999 Aug 25: Modified in accordance with current PANDORA
C                  conventions.
C     2001 Sep 24: changed for Solaris
C     2004 May 20: plot axis limit
C     2004 May 27: graph output file
C     2007 Apr 16: remove most of Pandora Development Environment
C                  (especially, re. Pandora common blocks) to
C                  make this a stand-alone application)
C     !BDECL
      real*8 Z,FLVS,FION,ZM,W,S,FLVSM,FIONM,RABDM,RABD,ZC,F,VERS
      integer NN,IMG,NI,NO,NP,JMAX,M,NMAX,IONMX,MMAX,LUEO,NG
      character LABEL*80,ELSYM*8,MARKS*1,IMAGE*8192
C     !EDECL
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence
     $(LUNITS( 6),LUEO )
C     !DASH
      external REED, MURGE, DISPERS, TELL, COMP, SHOW, CONSOL,
     $         GIVE, NORMAN, HELPIT, GRAFFI
C
      parameter (NMAX=300, IONMX=10, MMAX=900)
C     NMAX  = maximum number of depth points for any one data set
C     MMAX  = maximum number of entries in merged depths table
C     IONMX = maximum number of data sets
C
      dimension NN(IONMX), Z(NMAX,IONMX), FLVS(NMAX,IONMX),
     $          FION(NMAX,IONMX+1), ZM(MMAX), W(MMAX), S(MMAX),
     $          FLVSM(MMAX,IONMX), FIONM(MMAX,IONMX),
     $          RABDM(MMAX,IONMX), RABD(NMAX,IONMX), MARKS(MMAX),
     $          IMG(MMAX), ZC(NMAX), F(IONMX+1), LABEL(IONMX)
C
C     Data Input File     - 91
C     Listing Output File - 92
C     Data Output File    - 93
C     Graph Output File   - 94
C
      data NI,NO,NP,NG /91, 92, 93, 94/
C
      data VERS /6.02/
C     !EJECT
C     !beg
      LUEO = NO
C---- Read data
      call REED    (NI,NO,LABEL,ELSYM,NN,Z,FLVS,FION,JMAX,NMAX,IONMX)
C---- Make merged Z table
      call MURGE   (NN,Z,NMAX,JMAX,ZM,M,MMAX,W)
C---- Interpolate to merged Z table
      call DISPERS (NN,Z,NMAX,FION,FLVS,ZM,FIONM,FLVSM,M,MMAX,JMAX)
C---- Normalize data
      call NORMAN  (M,MMAX,JMAX,FIONM,FLVSM,S)
C---- Print data
      call TELL    (NO,LABEL,JMAX,M,MMAX,ZM,FIONM,FLVSM,S,
     $              NN,Z,NMAX,MARKS,IMAGE,ZC,'input')
C---- Edit the data
      call HELPIT  (JMAX,F,M,MMAX,FIONM,FLVSM,S)
C---- Print data
      call TELL    (NO,LABEL,JMAX,M,MMAX,ZM,FIONM,FLVSM,S,
     $              NN,Z,NMAX,MARKS,IMAGE,ZC,'edited')
C---- Output file for degree-of-ionization graph
      call GRAFFI  (NG,LABEL,JMAX,M,MMAX,ZM,FIONM,FLVSM,S)
C---- Compute abundance ratios
      call COMP    (M,MMAX,FIONM,FLVSM,JMAX,RABDM,NO,IMG)
C---- Print results
      call SHOW    (NO,JMAX,M,MMAX,ZM,RABDM,NN,Z,NMAX,MARKS,ELSYM,VERS,
     $              IMAGE,ZC)
C---- Interpolate back to individual Z tables of input data sets
      call CONSOL  (M,MMAX,ZM,RABDM,JMAX,NN,NMAX,Z,RABD)
C---- Write out data for use by PANDORA
      call GIVE    (NP,NO,ELSYM,NN,NMAX,Z,RABD,JMAX)
C
      stop 'census: done'
C     !end
      end
      subroutine REED
     $(NI,NO,LABEL,ELSYM,NN,Z,FLVS,FION,JMAX,NMAX,IONMX)
C
C     Rudolf Loeser, 1983 Jun 07
C     !DASH
      save
C     !BDECL
      real*8 Z,FLVS,FION,DUMMY
      integer NI,NO,NN,JMAX,NMAX,IONMX,JUMMY,MODE,N,NCAR,IONST
      character LABEL*80,ELSYM*8,HEAD*80,QUMMY*8,SYM*8
C     !EDECL
C     !COM
C---- DATAFIL     as of 1984 Apr 19
      integer     KIWILFN
      common      /DATAFIL/ KIWILFN
C     Number of unit from which to read input statements.
C     .
C---- CARDFIL     as of 1984 Apr 19
      integer     KIWIOUT
      common      /CARDFIL/ KIWIOUT
C     Number of unit to which to copy input statements.
C     .
C---- INCARD      as of 1984 Apr 23
      integer     LAST,LUINP,IRD,IPR,LUOUT,KARD,KART
      character   BUFFER*80
      common      /MAORI/  BUFFER
      common      /KAURI/  LAST
      common      /NUDEEL/ LUINP,IRD,IPR,LUOUT,KARD,KART
C     Storage and controls for reading input statements:
C     BUFFER - input line buffer;
C     LAST   - input buffer scan pointer;
C     LUINP,IRD,IPR,LUOUT,KARD,KART - "NUDEAL" control
C     parameters, q.v.
C     .
C     !DASH
      external  ABJECT, LINER, KIWI, ABORT, RARE
      intrinsic max
C
      dimension Z(NMAX,*), FLVS(NMAX,*), FION(NMAX,*), NN(*),
     $          LABEL(*)
C     !EJECT
C     !beg
      LAST  = -1
      LUINP = 0
      IRD   = 0
      IPR   = 0
      LUOUT = 0
      KARD  = 0
      KART  = 0
      KIWILFN = NI
      KIWIOUT = NO
      JMAX    = 0
      ELSYM   = 'ZZ'
C---- Loop over all data sets
  100 continue
C----   Read and print header
        read (NI,101,END=103) HEAD
  101   format(A80)
        call LINER (1,NO)
        write (NO,102) HEAD
  102   format(' ','************** ',A80)
C----   Set initialization signal for NUDEAL
        IRD = 1
C----   Read and check element symbol
        call KIWI  (MODE,DUMMY,JUMMY,SYM,NCAR)
        if(MODE.ne.2) call ABORT
        if(ELSYM.eq.'ZZ') then
          ELSYM = SYM
        else
          if(ELSYM.ne.SYM) call ABORT
        end if
C----   Read and check ionization stage
        call KIWI  (MODE,DUMMY,IONST,QUMMY,NCAR)
        if(MODE.ne.3) call ABORT
        if((IONST.le.0).or.(IONST.gt.IONMX)) call ABORT
        JMAX = max(JMAX,IONST)
C----   Read and check number of depth points
        call KIWI  (MODE,DUMMY,N,QUMMY,NCAR)
        if(MODE.ne.3) call ABORT
        if((N.le.0).or.(N.gt.NMAX)) call ABORT
C----   Accept LABEL and N
        LABEL(IONST) = HEAD
        NN(IONST)    = N
C----   Read Z table
        call RARE  (Z(1,IONST),N,'Z',' ')
C----   Read FLVS values
        call RARE  (FLVS(1,IONST),N,'FLVS','FLVSL')
C----   Read FION table
        call RARE  (FION(1,IONST),N,'FION','FIONL')
      goto 100
C---- EOF is signal for no more data sets
  103 continue
C     !end
      return
      end
      subroutine RARE
     $(ARRAY,N,NAME1,NAME2)
C
C     Rudolf Loeser, 1983 Jun 09
C     !DASH
      save
C     !BDECL
      real*8 ARRAY,DUMMY,TEN
      integer N,JUMMY,MODE,KODE,I,NCAR
      character SYM*8,NAME1*(*),NAME2*(*)
C     !EDECL
C     !DASH
      external KIWI, ABORT, ARRAN
C
      dimension ARRAY(*)
C
      data TEN /1.D1/
C     !beg
C---- Read an array in PANDORA format:
C     first, the name,
      call KIWI (MODE,DUMMY,JUMMY,SYM,NCAR)
      if(MODE.ne.2) call ABORT
      if(SYM.eq.NAME1) then
        KODE = 1
      else if(SYM.eq.NAME2) then
        KODE = 2
      else
        call ABORT
      end if
C     then, the opening paren,
      call KIWI (MODE,DUMMY,JUMMY,SYM,NCAR)
      if(MODE.ne.2) call ABORT
      if(SYM.ne.'(') call ABORT
C     and finally, the values of the array.
      call ARRAN (1,ARRAY,JUMMY,N,SYM)
      if(KODE.eq.2) then
C----   Convert to antilogs
        do 100 I = 1,N
          ARRAY(I) = TEN**ARRAY(I)
  100   continue
      end if
C     !end
      return
      end
      subroutine MURGE
     $(NN,Z,NMAX,JMAX,ZM,M,MMAX,W)
C
C     Rudolf Loeser, 1983 Jun 07
C     !DASH
      save
C     !BDECL
      real*8 Z,ZM,W
      integer NN,NMAX,JMAX,M,MMAX,J
C     !EDECL
C     !DASH
      external MERGIT
C
      dimension NN(*), Z(NMAX,*), ZM(*)
C     !beg
C---- Initialize length of merged table
      M = 0
C---- Loop over all data sets
      do 100 J = 1,JMAX
C----   Merge J'th Z table into evolving merged table
        call MERGIT (Z(1,J),NN(J),ZM,M,MMAX,W)
  100 continue
C     !end
      return
      end
      subroutine MERGIT
     $(Z,N,ZM,M,MMAX,W)
C
C     Rudolf Loeser, 1983 Jun 07
C     !DASH
      save
C     !BDECL
      real*8 Z,ZM,W
      integer N,M,MMAX,K,KODE
C     !EDECL
C     !DASH
      external ABORT, MOVED, MERGED, ELIM
C
      dimension ZM(*), Z(*)
C     !beg
C---- Set up latest version of ZM in intermediate vector W
      if(M.eq.0) then
C----   ZM is empty - initialize with current Z
        K = N
        if(K.gt.MMAX) call ABORT
        call MOVED  (Z,1,N,W,1,K)
      else
C----   Merge current Z with evolving ZM
        call MERGED (Z,N,ZM,M,W,K,MMAX,KODE)
        if(KODE.ne.1) call ABORT
      end if
C---- Copy W back into ZM, omitting any duplicate values
      call ELIM     (W,K,ZM,M)
C     !end
      return
      end
      subroutine ELIM
     $(W,K,Z,M)
C
C     Rudolf Loeser, 1983 Jun 07
C     !DASH
      save
C     !BDECL
      real*8 W,Z,DELTA
      integer K,M,FLAG,I
C     !EDECL
C     !DASH
      external COMPD
C
      dimension W(K), Z(*)
C
      data DELTA /1.D-10/
C     !beg
      M    = 1
      Z(M) = W(1)
      do 100 I = 2,K
        call COMPD (W(I),Z(M),DELTA,FLAG)
        if(FLAG.eq.1) then
          M    = M+1
          Z(M) = W(I)
        end if
  100 continue
C     !end
      return
      end
      subroutine DISPERS
     $(NN,Z,NMAX,FION,FLVS,ZM,FIONM,FLVSM,M,MMAX,JMAX)
C
C     Rudolf Loeser, 1983 Jun 08
C     !DASH
      save
C     !BDECL
      real*8 Z,FION,FLVS,ZM,FIONM,FLVSM,ZERO,FLOOR,BIG
      integer NN,NMAX,M,MMAX,JMAX,J,N,I
C     !EDECL
C     !DASH
      external DERE, ABORT
C
      dimension Z(NMAX,*),NN(*),FION(NMAX,*),FLVS(NMAX,*),
     $          FIONM(MMAX,*),FLVSM(MMAX,*)
C
      data FLOOR,BIG /-1.D6, 3.D2/
      data ZERO /0.D0/
C     !beg
C---- Loop over all stages of ionization
      do 102 J = 1,JMAX
        N = NN(J)
C----   Convert to logs
        do 100 I = 1,N
          if(FION(I,J).gt.ZERO) then
            FION(I,J) = log(FION(I,J))
          else
            FION(I,J) = FLOOR
          end if
          if(FLVS(I,J).gt.ZERO) then
            FLVS(I,J) = log(FLVS(I,J))
          else
            FLVS(I,J) = FLOOR
          end if
  100   continue
C----   Interpolate input tables to merged Z table
        call DERE (Z(1,J),1,FION(1,J),1,N,ZM,1,FIONM(1,J),1,M,2)
        call DERE (Z(1,J),1,FLVS(1,J),1,N,ZM,1,FLVSM(1,J),1,M,2)
C----   Take antilogs
        do 101 I = 1,M
          if(FIONM(I,J).gt.BIG) call ABORT
          if(FLVSM(I,J).gt.BIG) call ABORT
          FIONM(I,J) = exp(FIONM(I,J))
          FLVSM(I,J) = exp(FLVSM(I,J))
  101   continue
C----   (Note: all values of FIONM and FLVSM are now > zero!)
  102 continue
C     !end
      return
      end
      subroutine NORMAN
     $(M,MMAX,JMAX,FIONM,FLVSM,S)
C
C     Rudolf Loeser, 1983 Jun 10
C     !DASH
      save
C     !BDECL
      real*8 FIONM,FLVSM,S,ZERO,ONE,SUM,FAC
      integer M,MMAX,JMAX,I,J
C     !EDECL
C     !DASH
      intrinsic max
C
      data ZERO,ONE /0.D0, 1.D0/
C
      dimension FIONM(MMAX,*), FLVSM(MMAX,*), S(MMAX)
C     !beg
C---- Loop over all depths
      do 102 I = 1,M
C----   Compute sum of fractions in all stages of ionization
        SUM = FLVSM(I,1)+FIONM(I,JMAX)
        if(JMAX.ge.2) then
          do 100 J = 2,JMAX
            SUM = SUM+max(FIONM(I,J-1),FLVSM(I,J))
  100     continue
        end if
        S(I) = SUM
C----   Normalize all data points
        if((SUM.gt.ZERO).and.(SUM.lt.ONE)) then
          FAC = ONE/SUM
          do 101 J = 1,JMAX
            FIONM(I,J) = FAC*FIONM(I,J)
            FLVSM(I,J) = FAC*FLVSM(I,J)
  101     continue
        end if
  102 continue
C     !end
      return
      end
      subroutine TELL
     $(NO,LABEL,JMAX,M,MMAX,ZM,FIONM,FLVSM,S,NN,Z,NMAX,MARKS,IMAGE,
     $ ZC,MODE)
C
C     Rudolf Loeser, 1983 Jun 08
C     !DASH
      save
C     !BDECL
      real*8 ZM,FIONM,FLVSM,Z,ZC,S
      integer NO,JMAX,M,MMAX,NN,NMAX,J,IE,IS,I
      character LABEL*80, MAST*8, STAGE*7, LL*1, II*1, BL*1, MARKS*1
      character SS*8, IMAGE*(*), MODE*(*)
C     !EDECL
C     !DASH
      external  ABJECT, LINER, MURK, PLOTLI
      intrinsic min
C
      dimension ZM(MMAX), FIONM(MMAX,*), FLVSM(MMAX,*), Z(NMAX,*),
     $          NN(*), S(MMAX), ZC(MMAX), LABEL(*), MARKS(*)
C
      data MAST,LL,II,BL,SS /'MASTER Z', 'L', 'I', ' ', 'S'/
C     !EJECT
C     !beg
      call ABJECT    (NO)
      write (NO,100) MODE
  100 format(' ',A,' Data from the following PANDORA runs is used:')
C---- Print headers
      call LINER     (1,NO)
      do 102 J = 1,JMAX
        write (NO,101) LABEL(J)
  101   format(' ',A80)
  102 continue
      call LINER     (1,NO)
C---- Print data, 8 depths at a time
      IE = 0
  103 continue
        IS = IE+1
        IE = min((IE+8),M)
        call LINER   (3,NO)
        write (NO,104) (I,I=IS,IE)
  104   format(' ',9X,8I15)
        write (NO,105)MAST,(ZM(I),BL,I=IS,IE)
  105   format(' ',A8,2X,8(1PE14.7,A1))
C----   Loop over all stages of ionization
        do 107 J = 1,JMAX
          write (STAGE,106) J,J+1
  106     format('(',I2,',',I2,')')
          call MURK  (ZM,IS,IE,Z(1,J),NN(J),MARKS)
          call LINER (1,NO)
          write (NO,105) LL//STAGE, (FLVSM(I,J),MARKS(I),I=IS,IE)
          write (NO,105) II//STAGE, (FIONM(I,J),MARKS(I),I=IS,IE)
  107   continue
        call LINER   (1,NO)
        write (NO,105) SS,(S(I),BL,I=IS,IE)
      if(IE.lt.M) goto 103
      call PLOTLI (IMAGE,M,MMAX,JMAX,FIONM,FLVSM,ZC,NO,MODE)
C     !end
      return
      end
      subroutine MURK
     $(ZM,IS,IE,Z,N,MARKS)
C
C     Rudolf Loeser, 1983 Jun 09
C     !DASH
      save
C     !BDECL
      real*8 ZM,Z,DELTA
      integer IS,IE,N,I,K,NOTE,LOOK
      character MARKS*1,STAR*1,BLANK*1
C     !EDECL
C     !DASH
      external LOOKSD
C
      dimension ZM(*), MARKS(*)
C
      data BLANK,STAR /' ', '*'/
      data DELTA /1.D-12/
C     !beg
C---- Loop over merged depths for current line
      do 100 I = IS,IE
C----   Check to see whether this merged Z value is part of the 
C       Z table of the data set for the current stage of ionization
        call LOOKSD (Z,N,DELTA,ZM(I),K,NOTE,LOOK)
        if(((LOOK.eq.1).and.(NOTE.eq.1)).or.(LOOK.eq.2)) then
C----     It is - mark with asterisk
          MARKS(I) = STAR
        else
C----     It is not - leave no mark
          MARKS(I) = BLANK
        end if
  100 continue
C     !end
      return
      end
      subroutine PLOTLI
     $(IMAGE,M,MMAX,JMAX,FIONM,FLVSM,ZC,NO,MODE)
C
C     Rudolf Loeser, 1999 Apr 14
C     !DASH
      save
C     !BDECL
      real*8 FIONM,FLVSM,ZC,YMIN,BOT,TOP,Y,ZERO,TEN,YLIM
      integer M,MMAX,JMAX,NO,N,I,J,IMINI,IMINL,JUMMY,NV,NH,K
      character IMAGE*(*),ISYM*1,LSYM*1,P*1,MODE*(*)
      logical OK
C     !EDECL
C     !DASH
      external  MINMAXD, BELOWLD, KINIT, GRIDLY, KPLOTC, QOOK,
     $          KPRINT, ABJECT, LINER
      intrinsic min, max
C
      dimension FIONM(MMAX,*), FLVSM(MMAX,*), ZC(*)
      dimension Y(18), ISYM(9), LSYM(9), P(18)
C
      data LSYM /'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'/
      data ISYM /'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'/
      data NV,NH /55, 117/
      data YLIM /1.D-300/
      data ZERO,TEN /0.D0, 1.D1/
C     !EJECT
C     !beg
      N = min(JMAX,9)
      do 100 I = 1,M
        ZC(I) = I
  100 continue
      YMIN = TEN
      do 101 J = 1,N
        call MINMAXD (FIONM(1,J),1,M,IMINI,JUMMY)
        call MINMAXD (FLVSM(1,J),1,M,IMINL,JUMMY)
        YMIN = min(YMIN,FIONM(IMINI,J),FLVSM(IMINL,J))
  101 continue
      call BELOWLD (YMIN,BOT)
      BOT = max(BOT,YLIM)
      BOT = log10(BOT)
      TOP = ZERO
      call KINIT (IMAGE,ZC(1),ZC(M),BOT,TOP,NV,NH,'#',OK)
      call GRIDLY (IMAGE,ZC(1),ZC(M),TEN,BOT,TOP,'.',1)
      do 103 I=1,M
        call QOOK (I,FIONM,ISYM,FLVSM,LSYM,MMAX,N,Y,P,K)
        do 102 J=1,K
          call KPLOTC (IMAGE,ZC(I),Y(J),P(J))
  102   continue
  103 continue
      call ABJECT (NO)
      write (NO,104) MODE
  104 format(' ','Plot of logs of L (lower case) and I (upper case) ',
     $           ' vs. Master Z index.',5X,A)
      call LINER  (1,NO)
      call KPRINT (IMAGE,NO)
C     !end
      return
      end
      subroutine QOOK
     $(I,FION,ISYM,FLVS,LSYM,MMAX,N,Y,P,K)
C
C     Rudolf Loeser, 1999 Apr 14
C     !DASH
      save
C     !BDECL
      real*8 FION,FLVS,Y,YI,YL
      integer I,MMAX,N,K,J
      character ISYM*1,LSYM*1,P*1
C     !EDECL
C     !DASH
      intrinsic mod
C
      dimension FION(MMAX,*), FLVS(MMAX,*), Y(*), ISYM(*),
     $          LSYM(*), P(*)
C     !beg
      K = 1
      Y(K) = log10(FLVS(I,1))
      P(K) = LSYM(1)
      do 100 J = 2,N
        YI = log10(FION(I,J-1))
        YL = log10(FLVS(I,J))
        if(mod(I,2).eq.1) then
          Y(K+1) = YI
          P(K+1) = ISYM(J-1)
          Y(K+2) = YL
          P(K+2) = LSYM(J)
        else
          Y(K+1) = YL
          P(K+1) = LSYM(J)
          Y(K+2) = YI
          P(K+2) = ISYM(J-1)
        end if
        K = K+2
  100 continue
      K = K+1
      Y(K) = log10(FION(I,N))
      P(K) = ISYM(N)
C     !end
      return
      end
      subroutine HELPIT
     $(JMAX,F,M,MMAX,FION,FLVS,S)
C
C     Rudolf Loeser, 1999 Apr 15
C     !DASH
      save
C     !BDECL
      real*8 FION,FLVS,S,F,ONE,DEN,RAT
      integer JMAX,M,MMAX,I,J
C     !EDECL
C     !DASH
      external DIVVY
C
      dimension FION(MMAX,*), FLVS(MMAX,*), S(*), F(*)
C
      data ONE /1.D0/
C     !beg
      do 103 I = 1,M
        DEN = ONE
        do 100 J = JMAX,1,-1
          call DIVVY (FION(I,J), FLVS(I,J), RAT)
          DEN = ONE+DEN*RAT
  100   continue
        F(1) = ONE/DEN
        S(I) = F(1)
        do 101 J = 1,JMAX
          call DIVVY (FION(I,J), FLVS(I,J), RAT)
          F(J+1) = F(J)*RAT
          S(I)   = S(I)+F(J+1)
  101   continue
        FLVS(I,1) = F(1)
        do 102 J = 2,JMAX
          FION(I,J-1) = F(J)
          FLVS(I,J  ) = F(J)
  102   continue
        FION(I,JMAX) = F(JMAX+1)
  103 continue
C     !end
      return
      end
      subroutine GRAFFI
     $(NG,LABEL,JMAX,M,MMAX,Z,FION,FLVS,AVE)
C
C     Rudolf Loeser, 2004 May 26
C---- Writes degree-of-ionization data to special file.
C     !DASH
      save
C     !BDECL
      real*8 Z,FION,FLVS,AVE,TWO
      integer NG,JMAX,M,MMAX,J,I
      character LABEL*80
C     !EDECL
C     !DASH
      external PUN
C
      dimension Z(*), LABEL(*), FION(MMAX,*), FLVS(MMAX,*), AVE(*)
C
      data TWO /2.D0/
C     !beg
      rewind NG
      write (NG,100) M,(JMAX+1)
  100 format(2I10)
      write (NG,101) (LABEL(J),J=1,JMAX)
  101 format(A)
      write (NG,101) 'Z'
      call PUN (Z,1,M,1,NG)
      write (NG,101) 'TE'
      do 103 J = 1,JMAX
        write (NG,100) J
        if(J.eq.1) then
          call PUN (FLVS(1,J),1,M,1,NG)
        end if
        if(J.ge.2) then
          do 102 I = 1,M
            AVE(I) = (FLVS(I,J)+FION(I,J-1))/TWO
  102     continue
          call PUN (AVE,1,M,1,NG)
        end if
        if(J.eq.JMAX) then
          write (NG,100) (J+1)
          call PUN (FION(1,J),1,M,1,NG)
        end if
  103 continue
C     !end
      return
      end
      subroutine COMP
     $(M,MMAX,XI,XL,JMAX,XR,NO,IMG)
C
C     Rudolf Loeser, 1983 Jun 09
C     !DASH
      save
C     !BDECL
      real*8 XI,XL,XR,ZERO,ONE,CRIT,RMIN,ZLL,ZIL,ZLH,ZIH,RA,TM,RB,R
      integer M,MMAX,JMAX,NO,IMG,J,I,K,LF,LL,L
      character LABEL*7
      logical NOTFRST,NOTLAST,BAD,ALLBAD
C     !EDECL
C     !DASH
      external  ABORT, EDIT1
      intrinsic max
C
      dimension XI(MMAX,*), XL(MMAX,*), XR(MMAX,*), IMG(MMAX)
C
      data RMIN,CRIT /1.D-12, 1.D-2/
      data KERM,NERM /0,100/
      data ZERO,ONE /0.D0, 1.D0/
C     !EJECT
C     !beg
C---- Loop over all stages of ionization
      do 105 J = 1,JMAX
        NOTFRST = J.gt.1
        NOTLAST = J.lt.JMAX
C----   Loop over all depths
        do 103 I = 1,M
C----     Method A:
C----     using data from adjacent stages only
          K = 0
          if(NOTFRST) then
            ZLL = XL(I,J-1)
            ZIL = XI(I,J-1)
          else
            ZLL = ZERO
            ZIL = ZERO
          end if
          if(NOTLAST) then
            ZLH = XL(I,J+1)
            ZIH = XI(I,J+1)
          else
            ZLH = ZERO
            ZIH = ZERO
          end if
          if((ZLL+ZIL+ZLH+ZIH).eq.ZERO) then
            RA = ZERO
            K  = 1
          else if((ZLL+ZIL).gt.ZERO) then
            if(ZIL.le.CRIT*ZLL) then
              RA = ZIL
              K  = 1
            end if
          else if((ZLH+ZIH).gt.ZERO) then
            if(ZLH.le.CRIT*ZIH) then
              RA = ZLH
              K  = 1
            end if
          end if
C     !EJECT
C----     Method B:
C----     using data from all lower stages and all higher stages.
          TM = ONE
          LF = 1
          LL = J-1
          if(LL.ge.LF) then
            do 101 L = LF,LL
C             TM=TM-XL(I,L)    changed 1999 Apr 07
              if(L.eq.LF) then
                TM = TM-XL(I,L)
              else
                TM = TM-XI(I,L-1)
              end if
  101       continue
          end if
          LF = J+1
          LL = JMAX
          if(LL.ge.LF) then
            do 102 L = LF,LL
C             TM=TM-XI(I,L)    changed 1999 Apr 07
              if(L.eq.LL) then
                TM = TM-XI(I,L)
              else
                TM = TM-XL(I,L+1)
              end if
  102       continue
          end if
          RB = max(TM,ZERO)
C----     Choose result
          R = RB
          if((R.lt.RMIN).and.(K.eq.1)) then
            R = RA
          end if
          XR(I,J) = R
  103   continue
C----   Edit out zeroes
        write (LABEL,104) J
  104   format('RABD',I3)
        call EDIT1 (XR(1,J),M,ZERO,2,1,IMG,BAD,ALLBAD)
  105 continue
C     !end
      return
      end
      subroutine SHOW
     $(NO,JMAX,M,MMAX,ZM,RABDM,NN,Z,NMAX,MARKS,ELSYM,VERS,IMAGE,ZC)
C
C     Rudolf Loeser, 1983 Jun 08
C     !DASH
      save
C     !BDECL
      real*8 ZM,RABDM,ZC,Z,VERS
      integer NO,JMAX,M,MMAX,NN,NMAX,IE,IS,I,J
      character STAGE*7,RR*1,MAST*8,DAT*11,TIM*8,MARKS*1,BL*1,ELSYM*8
      character IMAGE*(*)
C     !EDECL
C     !DASH
      external  ABJECT, LINER, MURK, get_date, get_time, PLOTR
      intrinsic min
C
      dimension ZM(MMAX), RABDM(MMAX,*), Z(NMAX,*), NN(*),ZC(*),
     $          MARKS(*)
C
      data MAST,RR,BL /'MASTER Z', 'R', ' '/
C     !beg
      call ABJECT   (NO)
      call get_date (DAT)
      call get_time (TIM)
      write (NO,100) ELSYM(1:2),DAT,TIM,VERS
  100 format(' ','Computed Abundance Ratios for ',A2,10X,A11,2X,A8,
     $           10X,'Census ',F5.2)
C
C---- Print results, 8 depths at a time
      IE = 0
  101 continue
        IS = IE+1
        IE = min((IE+8),M)
        call LINER  (3,NO)
        write (NO,102) (I,I=IS,IE)
  102   format(' ',9X,8I15)
        write (NO,103) MAST,(ZM(I),BL,I=IS,IE)
  103   format(' ',A8,2X,8(1PE14.7,A1))
        call LINER  (1,NO)
C
        do 105 J = 1,JMAX
          write (STAGE,104) J,J+1
  104     format('(',I2,',',I2,')')
          call MURK (ZM,IS,IE,Z(1,J),NN(J),MARKS)
          write (NO,103) RR//STAGE, (RABDM(I,J),MARKS(I),I=IS,IE)
  105   continue
C
      if(IE.lt.M) goto 101
      call PLOTR (IMAGE,M,MMAX,JMAX,RABDM,ZC,NO)
C     !end
      return
      end
      subroutine PLOTR
     $(IMAGE,M,MMAX,JMAX,R,ZC,NO)
C
C     Rudolf Loeser, 1999 Apr 14
C     !DASH
      save
C     !BDECL
      real*8 R,ZC,ZERO,TEN,YMIN,BOT,TOP,Y,YLIM
      integer M,MMAX,JMAX,NO,IMIN,JUMMY,N,I,J,NV,NH
      character IMAGE*(*),RSYM*1
      logical OK
C     !EDECL
C     !DASH
      external  MINMAXD, BELOWLD, KINIT, GRIDLY, KPLOTC, KPRINT,
     $          ABJECT, LINER
      intrinsic min, max
C
      dimension R(MMAX,*), ZC(*), RSYM(9)
C
      data RSYM /'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'/
      data NV,NH /55, 117/
      data YLIM /1.D-300/
      data ZERO,TEN /0.D0, 1.D1/
C     !EJECT
C     !beg
      N = min(JMAX,9)
      do 100 I = 1,M
        ZC(I) = I
  100 continue
      YMIN = TEN
      do 101 J = 1,N
        call MINMAXD (R(1,J),1,M,IMIN,JUMMY)
        YMIN = min(YMIN,R(IMIN,J))
  101 continue
      call BELOWLD (YMIN,BOT)
      BOT = max(YLIM,BOT)
      BOT = log10(BOT)
      TOP = ZERO
      call KINIT (IMAGE,ZC(1),ZC(M),BOT,TOP,NV,NH,'#',OK)
      call GRIDLY (IMAGE,ZC(1),ZC(M),TEN,BOT,TOP,'.',1)
      do 103 J = 1,N
        do 102 I = 1,M
          Y = log10(R(I,J))
          call KPLOTC (IMAGE,ZC(I),Y,RSYM(J))
  102   continue
  103 continue
      call ABJECT (NO)
      write (NO,104)
  104 format(' ','Plot of logs of RABD vs. Master Z index.')
      call LINER  (1,NO)
      call KPRINT (IMAGE,NO)
C     !end
      return
      end
      subroutine CONSOL
     $(M,MMAX,ZM,RABDM,JMAX,NN,NMAX,Z,RABD)
C
C     Rudolf Loeser, 1983 Jun 08
C     !DASH
      save
C     !BDECL
      real*8 ZM,RABDM,Z,RABD
      integer M,MMAX,JMAX,NN,NMAX,J
C     !EDECL
C     !DASH
      external DERE
C
      dimension NN(*), RABDM(MMAX,*), Z(NMAX,*), RABD(NMAX,*)
C     !beg
C---- Loop over all stages of ionization
      do 100 J = 1,JMAX
C----   Select from comprehensive table the values needed for
C       the Z table of the current data set
        call DERE (ZM,1,RABDM(1,J),1,M,Z(1,J),1,RABD(1,J),1,NN(J),2)
  100 continue
C     !end
      return
      end
      subroutine GIVE
     $(NP,NO,ELSYM,NN,NMAX,Z,RABD,JMAX)
C
C     Rudolf Loeser, 1983 Jun 08
C     !DASH
      save
C     !BDECL
      real*8 Z,RABD,SIG,CRIT
      integer NP,NO,NN,NMAX,JMAX,J,I
      character ELSYM*8,LINE*80,ROMAN*4
C     !EDECL
C     !DASH
      external PUN, ABJECT
C
      dimension NN(*), Z(NMAX,*), RABD(NMAX,*), ROMAN(10)
C
      data ROMAN /'I   ', 'II  ', 'III ', 'IV  ', 'V   ',
     $            'VI  ', 'VII ', 'VIII', 'IX  ', 'X   '/
      data SIG,CRIT /-3.D2, 1.D-300/
C     !EJECT
C     !beg
C---- Convert to logs
      do 201 J=1,JMAX
        do 200 I=1,NN(J)
          if(RABD(I,J).le.CRIT) then
            RABD(I,J) = SIG
          else
            RABD(I,J) = log10(RABD(I,J))
          end if
  200   continue
  201 continue
C
      rewind NP
C---- Loop over all data sets
      do 101 J = 1,JMAX
C----   Output this RABD table, in PANDORA format
        write (NP,100) ELSYM(1:2),ROMAN(J)
  100   format('[ To use with ',A2,'-',A4,' ]  RABDL     ( > ')
        call PUN  (RABD(1,J),1,NN(J),1,NP)
        write (NP,199)
  199   format(') > ')
  101 continue
C---- Now, copy to regular output file
      rewind NP
      call ABJECT (NO)
  102 continue
        read (NP,103,END=105) LINE
  103   format(A80)
        write (NO,104) LINE
  104   format(' ',A80)
        goto 102
  105 continue
C     !end
      return
      end
      subroutine ABORT
C
C     Rudolf Loeser, 1996 Jul 22
C---- Stops the run in case of error.
C     !DASH
      save
C     !DASH
C     !beg
      stop 'ABORT: run stopped because of an error.'
C     !end
      end
      subroutine GRIDLY
     $(IMAGE,CL,CU,DC,A,B,SYM,MODE)
C
C     Rudolf Loeser, 1970 Jan 28
C---- Enters grid lines.
C     Inserts lines for all values between CL and CU
C     which are integral multiples of DC.
C     MODE=1: X constant; =2: Y constant.
C     !DASH
      save
C     !BDECL
      real*8 CL,CU,DC,A,B,ZERO,ZL,ZU,ZLDC,C
      integer MODE,IZLDC
      character SYM*1,IMAGE*(*)
C     !EDECL
C     !DASH
      external  KLINEC
      intrinsic max,min
C
      data ZERO /0.D0/
C     !beg
      ZL    = min(CL,CU)
      ZU    = max(CL,CU)
      IZLDC = ZL/DC
      ZLDC  = IZLDC
      C     = DC*ZLDC
      if((C.ne.ZL).and.(ZL.lt.ZERO)) then
        C = C-DC
      end if
C
  100 continue
        C = C+DC
        if(C.lt.ZU) then
          if(MODE.eq.1) then
            call KLINEC (IMAGE,C,A,C,B,SYM,0)
            goto 100
          else
            call KLINEC (IMAGE,A,C,B,C,SYM,0)
            goto 100
          end if
        end if
C     !end
      return
      end
      subroutine PUN
     $(A,IS,IE,INC,LU)
C
C     Rudolf Loeser, 1981 Jun 02
C---- Writes the values of A, for SPUN.
C     (See also PUD.)
C     (This is version 2 of PUN.)
C     !DASH
      save
C     !BDECL
      real*8 A
      integer IS,IE,INC,LU,I,J
      character F*16,BLANK*1
C     !EDECL
C     !DASH
      external PPUN
C
C               A(m,INC)
      dimension A(*)
C
      dimension F(5)
      data BLANK /' '/
C     !beg
      J = IS-INC
  100 continue
C
        do 101 I = 1,5
          J = J+INC
          if(J.le.IE) then
            call PPUN (A(J),F(I))
          else
            F(I) = BLANK
          end if
  101   continue
C
        write (LU,102) F
  102   format(5A16)
C
      if(J.lt.IE) goto 100
C     !end
      return
      end
      subroutine PPUN
     $(A,F)
C
C     Rudolf Loeser, 2001 Jan 19
C---- Encodes a value of A, for PUN.
C     !DASH
      save
C     !BDECL
      real*8 A,ZERO
      character F*16,BLANK*1,E1*1,E2*1,D1*1,D2*1,X*1
      logical XOK
C     !EDECL
C     !DASH
      data E1, E2, D1, D2 /'E', 'e', 'D', 'd'/
      data ZERO,BLANK /0.D0, ' '/
C     !beg
      write (F,100) A
  100 format(1PE15.8)
      if(A.ne.ZERO) then
        X   = F(12:12)
        XOK = (X.eq.E1).or.(X.eq.E2).or.(X.eq.D1).or.(X.eq.D2)
        if(.not.XOK) then
          F(11:11) = E1
        end if
      end if
C     !end
      return
      end
      subroutine ARRAN
     $(LODE,FLOATS,INTS,N,NAME)
C
C     Rudolf Loeser, 1983 Oct 03
C---- Reads floating or integer arrays, length N, using KIWI and NUDEAL.
C     (Remember that the behavior of NUDEAL is in part controlled
C     by the labelled common blocks declared in KIWI.)
C
C     The input parameter LODE specifies the type of array to be read:
C     LODE=1: floating point (R*8); LODE=2: integer (I*4).
C
C---- The operation of ARRAN is described in: "About PANDORA".
C     !DASH
      save
C     !BDECL
      real*8 FLOATS,FVAL,ONE
      integer LODE,INTS,N,LUEO,KERR,jummy,IVAL,MODE
      character NAME*8,ALF*8,GO*8,MESS*5
C     !EDECL
C     !COM
C---- IONA        as of 2001 Sep 10
      real*8      FM
      integer     K,IM,IN,KIND
      common      /IONA1/ FM
      common      /IONA2/ K,IM,IN,KIND
C     Control parameters for ARRAN's subroutines - read r*8 arrays.
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
C     .
      equivalence
     $(LUNITS( 6),LUEO )
C     !DASH
      external COCK, BUTE, CHLOE, LINER, ABORT, HALT, MESHED, CARMEN,
     $         KIWI, HI, BYE
C
C               FLOATS(N), INTS(N)
      dimension FLOATS(*), INTS(*)
C
      dimension MESS(2)
C
      data GO   /')'/
      data MESS /'FLTPT', 'INTGR'/
      data ONE /1.D0/
C     !EJECT
C     !beg
      if((LODE.lt.1).or.(LODE.gt.2)) then
        write (MSSLIN(1),100) LODE
  100   format('LODE =',I12,', which is neither 1 nor 2.')
        call HALT   ('ARRAN', 1)
      end if
C
C---- Initialize
      KERR = 0
      IN = 1
      FM = ONE
      IM = 1
      K  = 1
  101 continue
C----   Obtain next input field
        call KIWI   (MODE, FVAL, IVAL, ALF, jummy)
C----   Branch on mode of field
C
C             null      alpha     int       toobig    fltpt
        goto (207,      102,      103,      208,      103       ), MODE
C
  102   continue
C----     Alpha field must be control field - identify it,
C         and jump to appropriate processing section according to type
          KIND = 0
C
          if((ALF.eq.'M').or.(ALF.eq.'m')) KIND = 1
          if((ALF.eq.'F').or.(ALF.eq.'f')) KIND = 2
          if(KIND.gt.0) goto 103
C
          if((ALF.eq.'R').or.(ALF.eq.'r')) KIND = 1
          if((ALF.eq.'I').or.(ALF.eq.'i')) KIND = 2
          if((ALF.eq.'S').or.(ALF.eq.'s')) KIND = 3
C
          if(KIND.gt.0) then
            goto 104
          end if
C
          if(ALF.eq.GO) then
            goto 199
          else
            goto 202
          end if
C
  103   continue
C----     Process array values
          call COCK (MODE, LODE, FVAL, IVAL, FLOATS, INTS, N, NAME)
          goto 101
C
  104   continue
C----     Process array controls, and comments
          call BUTE (NAME)
          goto 101
C     !EJECT
C---- Process errors
  208 KERR = KERR+1
  207 KERR = KERR+1
  206 KERR = KERR+1
  205 KERR = KERR+1
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED   ('ARRAN', 1)
      write (LUEO,200) MESS(LODE),NAME,'R','r','I','i','S','s',
     $                 'M','m','F','f',GO
  200 format(' ','Error reading ',A5,' array: ',A8//
     $       ' ','List of valid control fields:'//(' ',4X,10A10))
      call CHLOE    (LUEO, NAME, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !end
      return
      end
      subroutine CARMEN
C
C     Rudolf Loeser, 1996 Dec 10
C---- CARMEN is called during input reading after ABORT,
C     which is nonsense except when "delayed ABORT" is in effect.
C     The point of CARMEN is to get the input reading process back
C     on track by skipping to the nearest point from where it can
C     proceed: the end of the CURRENT input statement (but,
C     it might turn out to be end of the NEXT input statement---
C     can't be helped). However, CARMEN must not skip past "GO";
C     if "GO" is found, then proper action must be taken in the
C     appropriate input reading control routines.
C     !DASH
      save
C     !BDECL
      real*8 dummy
      integer jummy,MODE,LUEO
      character QPR*8,QGO*8,QFL*8
      logical FOUND
C     !EDECL
C     !COM
C---- CARGOT      as of 1996 Dec 10
      logical     ESCARGO
      common      /CARGOT/ ESCARGO
C     Input reading signal from subroutine CARMEN.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
C     .
      equivalence
     $(LUNITS( 6),LUEO )
C     !DASH
      external KIWI, LINER, NUTMEG, HI, BYE
C
      data QPR,QGO /')', 'GO'/
C     !beg
  100 continue
        call KIWI (MODE,dummy,jummy,QFL,jummy)
        ESCARGO = QFL.eq.QGO
        FOUND   = (MODE.eq.2).and.((QFL.eq.QPR).or.ESCARGO)
        if(.not.FOUND) goto 100
      continue
C
      call LINER  (2,LUEO)
      write (LUEO,101)
  101 format(' ','CARMEN''s attempt to reach end-of-statement ',
     $           'stopped on the following input line:')
      call LINER  (1,LUEO)
      call NUTMEG (LUEO,2)
      call LINER  (2,LUEO)
C     !end
      return
      end
      subroutine BUTE
     $(NAME)
C
C     Rudolf Loeser, 1983 Oct 03
C---- Processes control fields for ARRAN.
C     !DASH
      save
C     !BDECL
      real*8 dummy
      integer jummy,KERR,LUEO,MODE
      character ALF*8,NAME*8
C     !EDECL
C     !COM
C---- IONA        as of 2001 Sep 10
      real*8      FM
      integer     K,IM,IN,KIND
      common      /IONA1/ FM
      common      /IONA2/ K,IM,IN,KIND
C     Control parameters for ARRAN's subroutines - read r*8 arrays.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
C     .
      equivalence
     $(LUNITS( 6),LUEO )
C     !DASH
      external KIWI, CHLOE, MESHED, ABORT, CARMEN, HI, BYE
C     !EJECT
C     !beg
      KERR = 0
C
C           R    I    S 
      goto (100, 101, 102 ), KIND
C
  100 continue
C----   Process repeat specification
        call KIWI (MODE, dummy, K , ALF, jummy)
        if(MODE.ne.3) goto 203
        goto 199
  101 continue
C----   Process index specification
        call KIWI (MODE, dummy, IN, ALF, jummy)
        if(MODE.ne.3) goto 203
        goto 199
  102 continue
C----   Process skip specification
        IN = IN+K
        K  = 1
        goto 199
C
C---- Process errors
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('BUTE', 1)
      write (LUEO,200) NAME
  200 format(' ','Error reading for ',A8)
      call CHLOE  (LUEO, NAME, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !end
      return
      end
      subroutine COCK
     $(MODE,LODE,FVAL,IVAL,FLOATS,INTS,N,NAME)
C
C     Rudolf Loeser, 1983 Oct 03
C---- Processes data fields for ARRAN.
C     !DASH
      save
C     !BDECL
      real*8 FVAL,FLOATS,dummy
      integer MODE,LODE,IVAL,INTS,N,LUEO,KERR,jummy,IL
      character ALF*8,NAME*8
      logical FLPT
C     !EDECL
C     !COM
C---- IONA        as of 2001 Sep 10
      real*8      FM
      integer     K,IM,IN,KIND
      common      /IONA1/ FM
      common      /IONA2/ K,IM,IN,KIND
C     Control parameters for ARRAN's subroutines - read r*8 arrays.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
C     .
      equivalence
     $(LUNITS( 6),LUEO )
C     !DASH
      external  KIWI, CHLOE, MESHED, ABORT, CARMEN, HI, BYE
      intrinsic min
C
C               FLOATS(N), INTS(N)
      dimension FLOATS(*), INTS(*)
C     !EJECT
C     !beg
      KERR = 0
      FLPT = LODE.eq.1
      if(MODE.eq.2) then
C
C             M    F
        goto (100, 101 ), KIND
C
  100   continue
C----     Process multiplier specification
          if(FLPT) then
            call KIWI (MODE, FM, jummy, ALF, jummy)
            if(MODE.ne.5) goto 204
            goto 199
          else
            call KIWI (MODE, dummy, IM, ALF, jummy)
            if(MODE.ne.3) goto 203
            goto 199
          end if
  101   continue
C----     Process fill specification
          K = N
          goto 199
      else if(MODE.eq.3) then
C----   Process integer array element
        if(FLPT) goto 204
        IVAL = IM*IVAL
        goto 102
      else if(MODE.eq.5) then
C----   Process floating point array element
        if(.not.FLPT) goto 203
        FVAL = FM*FVAL
        goto 102
      else
        goto 209
      end if
  102 continue
C----   Store current value in array, K times
        if(IN.gt.N) goto 210
        IL = min((IN+(K-1)),N)
        do 103 K = IN,IL
          if(FLPT) then
            FLOATS(K) = FVAL
          else
            INTS  (K) = IVAL
          end if
  103   continue
        IN = IL+1
        K  = 1
        goto 199
C     !EJECT
C---- Error processing
  210 KERR = KERR+1
  209 KERR = KERR+1
  208 KERR = KERR+1
  207 KERR = KERR+1
  206 KERR = KERR+1
  205 KERR = KERR+1
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call MESHED ('COCK', 1)
      write (LUEO,200) NAME,N
  200 format(' ','Error reading for: ',A8,'  of length',I6)
      call CHLOE  (LUEO, NAME, KERR)
      call ABORT
      call CARMEN
C
C---- Go home
  199 continue
C     !end
      return
      end
      subroutine MESHED
     $(CALLER,N)
C
C     Rudolf Loeser, 2002 Oct 11
C---- Prints special output headers.
C     !DASH
      save
C     !BDECL
      integer N,LUEO
      character CALLER*(*)
C     !EDECL
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
C     .
      equivalence
     $(LUNITS( 6),LUEO )
C     !DASH
      external LINER, DASHER, MOSHED, HI, BYE
C     !beg
      if(N.eq.2) then
        write (LUEO,100) CALLER
  100   format(' ','$&%:',5X,A,15X,'Special Output')
      else if(N.eq.1) then
        call LINER (1, LUEO)
        write (LUEO,101) CALLER
  101   format(' ','$%&:',5X,A,15X,'Stop Request')
      else if(N.eq.3) then
        call LINER (1, LUEO)
        write (LUEO,102) CALLER
  102   format(' ','$%&:',5X,A,15X,'Message')
      end if
      call DASHER  (LUEO)
      call LINER   (1, LUEO)
C
      call MOSHED  (CALLER, 2)
C     !end
      return
      end
      subroutine MOSHED
     $(CALLER,KODE)
C
C     Rudolf Loeser, 2003 Dec 11
C---- Administers the MESHED/MASHED caller stack.
C     KODE = 1 means: pop;   KODE = 2 means: put.
C     !DASH
      save
C     !BDECL
      integer KODE
      character CALLER*(*),BLANK*1
C     !EDECL
C     !COM
C---- MISHED      as of 2003 Dec 11
      integer     MSHCNO,MSHLNG
      parameter   (MSHLNG=50)
      character   MSHCLR*40
      dimension   MSHCLR(MSHLNG)
      common      /MISHED1/ MSHCNO
      common      /MISHED2/ MSHCLR
C     Caller stack for MESHED/MASHED messaging system.
C     .
C     !DASH
      external HI, BYE
C
      data BLANK /' '/
C     !beg
      if(KODE.eq.2) then
C
        MSHCNO = MSHCNO+1
        if(MSHCNO.le.MSHLNG) then
          MSHCLR(MSHCNO) = CALLER
        end if
C
      else if(KODE.eq.1) then
C
        CALLER = BLANK
        if(MSHCNO.gt.0) then
          if(MSHCNO.le.MSHLNG) then
            CALLER = MSHCLR(MSHCNO)
          end if
          MSHCNO = MSHCNO-1
        end if
C
      end if
C     !end
      return
      end
      subroutine HALT
     $(CALLER,N)
C
C     Rudolf Loeser, 2002 Feb 25
C---- Prints error message and aborts.
C     !DASH
      save
C     !BDECL
      integer N,LUEO,I
      character CALLER*(*)
C     !EDECL
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
C     .
      equivalence
     $(LUNITS( 6),LUEO )
C     !DASH
      external MESHED, ABORT, HI, BYE
C     !beg
      call MESHED (CALLER,1)
C
      if((N.gt.0).and.(N.lt.5)) then
        do 101 I = 1,N
          write (LUEO,100) MSSLIN(I)
  100     format(' ',A)
  101   continue
      end if
C
      call ABORT
C     !end
      return
      end

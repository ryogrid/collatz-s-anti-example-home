[form1.frm]





Option Explicit
Public mx As Double
Public my As Double
Dim muPreserved As Preserved
Dim mblnEndFlag As Boolean             '終了処理を行って良いかのフラグ
Dim mintFoultCount As Integer          '接続が失敗した回数を保持
Dim mblnConnectionFlag As Boolean      '通信が可能か（接続が正常に介されているか）のフラグ
Dim lngProgramStartTime As Long         'プログラムが起動した時刻を保持（プログラム総起動時間算出用）
Dim nidSysInfo As NOTIFYICONDATA
Dim lRetVal As Long
Dim blnFormVisibleFlag As Boolean       '画面上にフォームが表示されているかを示すフラグ
Dim lngProcessID As Long                'Ｓｈｅｌｌ関数を用いる場合の戻り値であるプロセスＩＤを保持しておく
Dim mHwnd As Long                       'フォームのハンドルを保持
Dim blnPassFlag As Boolean               '計算開始のＩＦ文を抜けるためのflag

Private Sub Form_Load()
Dim intFilenumber
Dim work As Integer
Dim ProcessID As Long
      
        
        ProcessID = GetCurrentProcess                      '自プロセスのプロセスＩＤを得る
        Call SetPriorityClass(ProcessID, IDLE_PRIORITY_CLASS)  '自プロセスの優先度を下げる
        
        mHwnd = Form1.hwnd    'フォームのハンドルを取得する
        'shell_notifyiconＡＰＩのための初期値設定
        nidSysInfo.cbSize = Len(nidSysInfo)                        '構造体のサイズ
        nidSysInfo.hwnd = Form1.Picture1.hwnd                                  '通知されるウィンドウハンドル
        nidSysInfo.uID = 1                                         'ID
        nidSysInfo.uFlags = NIF_ICON Or NIF_TIP Or NIF_MESSAGE     'フラグ
        nidSysInfo.uCallbackMessage = WM_MBUTTONDOWN               'Calllback Message
        nidSysInfo.hIcon = Me.Icon                                 'タスクトレイに表示されるアイコン
        nidSysInfo.szTip = "Collatz's Anti-Exp＠ｈｏｍｅ" & vbNullChar                   'ToolTipに表示される文字列
        Form1.Visible = False
        lRetVal = Shell_NotifyIcon(NIM_ADD, nidSysInfo)            'タスクトレイにアイコンを格納する
        
        lngProgramStartTime = Timer
        Call LoadData
        If SpaceTrim(muPreserved.strName) = "" Then
                  work = MsgBox("初期設定をメニューから行って下さい", vbOKOnly + vbExclamation, "Collatz's Anti-Exp＠ｈｏｍｅ")
                  Call ShowForm
                  mblnEndFlag = True
                  Call mnuPrepare_Click
                  Exit Sub
        End If
        If muPreserved.blnAutoStartFlag = False And blnPassFlag = False Then              '自動計算を許可していない場合
                  work = MsgBox("計算を開始してよろしいですか？", vbYesNo, "Collatz's Anti-Exp＠ｈｏｍｅ")
                  If work = vbNo Then
                        mblnEndFlag = True
                        Label3.Caption = "待機中"
                        Label3.Left = (Form1.Width - Label3.Width) / 2
                        mnuExecute.Visible = True
                        Exit Sub
                  End If
        End If
        Winsock1.Close                                '接続を閉じる
        Winsock1.RemotePort = 1001                    'ポート番号設定
        Winsock1.LocalPort = 0

        If Left(muPreserved.strMustCulculate, 1) = "#" Or muPreserved.blnCulculateComplete = True Then
            mblnEndFlag = False
            Call Connect
        Else
            Call Calculate        '計算を行う
        End If
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
        
        If UnloadMode = 0 And mblnEndFlag = False Then        '閉じるボタンで閉じられて、かつ、mblnendflag=falseの場合終了をキャンセルする
               Cancel = 1
               Exit Sub
        End If
        Call EndWorking
End Sub

Private Sub mnuExecute_Click()
        blnPassFlag = True           'if文を抜けられるようにする
        mnuExecute.Visible = False
        Call Form_Load
End Sub

Private Sub mnuPrepare_Click()
Dim intFilenumber As Integer
Dim muPreserved As Preserved
Dim strexist As String
Dim intreturn As Integer
Dim work As Integer
                 
       If mblnEndFlag = True Then
            Timer3.Enabled = False
'            muPreserved.strAddres = InputBox("サーバのアドレスを入力して下さい", "アドレスの指定")
            muPreserved.strAddres = "myhomegrid.myhome.cx"
            muPreserved.strName = InputBox("ログインをする時のニックネームを入力して下さい", "ニックネームの登録")
            muPreserved.strMustCulculate = "#"                'まだ計算すべき値を得ていない事を示すように書き込む
            work = MsgBox("毎回、自動で計算を行う事を許可しますか？", vbYesNo, "自動計算許可")
            If work = vbYes Then
                   muPreserved.blnAutoStartFlag = True
            End If
            work = MsgBox("自動接続を許可しますか？", vbYesNo, "自動接続許可")
            If work = vbYes Then
                   muPreserved.blnAutoConnectConsent = True
            End If
            work = MsgBox("パソコンのスペック情報の送信を許可しますか？", vbYesNo, "パソコン情報送信許可")
            If work = vbYes Then
                   muPreserved.blnSpecSendConsent = True
            End If
            muPreserved.strMassageTo = InputBox("サーバ管理者にメッセージがあればお書きください(３０文字以内）", "メッセージ記録", "なし")
            intreturn = MsgBox("データファイルを書き換えてしまいますがよろしいですか？", vbOKCancel + vbCritical)
            If intreturn = vbOK Then
                Call Form1.FileCodeOut("Registry", xORnumber)
                intFilenumber = FreeFile
                Open App.Path & "\" & "Registry" For Random As intFilenumber Len = Len(muPreserved)
                    Put #intFilenumber, 1, muPreserved
                Close #intFilenumber
                Call Form1.FileCodeIn("Registry", xORnumber)             'ファイルを暗号化する
                mblnEndFlag = False
                Call Form_Load
            End If
       End If
End Sub

Private Sub mnuVeryHigh_Click()                        '優先度　"高い"をクリック時
Dim ProcessID As Long
        
        mnuHigh.Checked = False
        mnuNormal.Checked = False
        mnuVeryHigh.Checked = True
        ProcessID = GetCurrentProcess                      '自プロセスのプロセスＩＤを得る
        Call SetPriorityClass(ProcessID, HIGH_PRIORITY_CLASS)  '自プロセスの優先度を変更する
        Call ChangePriorityC(HIGH_PRIORITY_CLASS)   'Ｃ言語のＥＸＥが起動されている場合そちらも変更する
End Sub

Private Sub mnuHigh_Click()                              '優先度　"少し高め"をクリック時
Dim ProcessID As Long
        
        mnuNormal.Checked = False
        mnuVeryHigh.Checked = False
        mnuHigh.Checked = True
        ProcessID = GetCurrentProcess                      '自プロセスのプロセスＩＤを得る
        Call SetPriorityClass(ProcessID, NORMAL_PRIORITY_CLASS)  '自プロセスの優先度を変更する
        Call ChangePriorityC(NORMAL_PRIORITY_CLASS)   'Ｃ言語のＥＸＥが起動されている場合そちらも変更する
End Sub

Private Sub mnuNormal_Click()                             '優先度　"通常"をクリック時
Dim ProcessID As Long
        
        mnuVeryHigh.Checked = False
        mnuHigh.Checked = False
        mnuNormal.Checked = True
        ProcessID = GetCurrentProcess                      '自プロセスのプロセスＩＤを得る
        Call SetPriorityClass(ProcessID, IDLE_PRIORITY_CLASS)  '自プロセスの優先度を変更する
        Call ChangePriorityC(IDLE_PRIORITY_CLASS)   'Ｃ言語のＥＸＥが起動されている場合そちらも変更する
End Sub


Private Sub Timer2_Timer()                   '1分経過後に接続要求を試みるタイマー
        Timer2.Enabled = False
        mblnEndFlag = False
        Call Connect
End Sub

Private Sub Timer3_Timer()                 'データ送信が適切に成されたか監視するタイマー(返信がない場合、このタイマーによって強制終了する)
Dim work As Integer
        
        If mintFoultCount >= 5 Then                    'すでに５回接続を失敗してしまっている場合
            work = MsgBox("サーバーが応答しないため終了します", vbOKOnly, "Collatz's Anti-Exp＠ｈｏｍｅ")
            Call EndWorking
        Else
            Label3.Caption = "接続失敗・待機中"
            Label3.Left = (Form1.Width - Label3.Width) / 2
            Timer2.Enabled = True                      '一分後に再び接続を試みるタイマーを起動する
            mintFoultCount = mintFoultCount + 1
            mblnEndFlag = True
        End If
        Timer3.Enabled = False
End Sub

Private Sub Winsock1_Connect()
        mblnConnectionFlag = True
End Sub

Private Sub Winsock1_DataArrival(ByVal bytesTotal As Long)
Dim strDat As String
Dim intFilenumber As Integer
Dim valsplited As Variant
Static stcstrdat As String
Dim strexist As String

On Error Resume Next
        Winsock1.GetData strDat    'サーバからのデータ受信
        stcstrdat = stcstrdat & strDat
             If Right(strDat, 1) = "#" Then
                strDat = Left(stcstrdat, Len(stcstrdat) - 1)
                    strDat = OutCode(strDat, xORnumber)       'データを復号化する
                    If Left(strDat, 2) = "##" Then
                        Winsock1.Close
                        Timer3.Enabled = False         '強制終了タイマーを停止する
                        valsplited = Split(strDat, "!")
                        muPreserved.intFilenumber = Val(valsplited(1))
                        Winsock1.LocalPort = 0
                        muPreserved.blnCulculateComplete = False       '計算は終了していない（これからだという事を記録)
                        muPreserved.strMustCulculate = SpaceTrim(CStr(valsplited(2)))                          '計算を行う値を代入
                        muPreserved.intWorkCounts = CInt(valsplited(3))
                        muPreserved.intPrize = CInt(valsplited(4))
                        muPreserved.lngNowProgress = 0
                        Winsock1.Close
                        Label4.Caption = muPreserved.intWorkCounts & "回目のワークです：データ取得時の順位　" & muPreserved.intPrize & "位"
                        Label4.FontSize = Form1.Height * (72 / 1440) / 20
                        Label4.Left = 0
                        Label4.Top = 0
                        Call FileCodeOut("CulculateData", xORnumber)
                        strexist = Dir(App.Path & "\" & "CulculateData")            '計算結果の途中経過のファイルが存在する場合削除しておく
                        If strexist = "CulculateData" Then
                                  Kill App.Path & "\" & "CulculateData"
                        End If
                        Call Calculate
                    End If
                stcstrdat = ""
             End If
End Sub

Private Sub Timer1_Timer()                    'Ｃ言語のＥＸＥが計算を終了させたか監視するタイマーコントロール
Dim strWrited As String
Dim strCompleted As String
Dim intFilenumber As Integer
Dim lngNowProgress As Long
Dim work As Integer
Dim TimerStartTime As Long                    'タイマーの処理が始まった時刻を保持

         On Error GoTo Errorhandler
         TimerStartTime = Timer
         Do While (Timer - TimerStartTime < 0.5)        '0.5秒間、ＯＳに制御を戻す
          DoEvents
         Loop
         intFilenumber = FreeFile
         Open App.Path & "\" & "CulculateData" For Random As intFilenumber Len = Len(lngNowProgress)
                Get #intFilenumber, 1, lngNowProgress             'Ｃ言語で書き込まれたファイルから現在の計算している値を得る、これが０の場合
            If lngNowProgress = -1 Or lngNowProgress = -2 Then              '1加算してあるので２は真、１は偽として取得する                                                                 '計算終了を示す
               mblnEndFlag = False               '終了処理を再び不可にする
               Close #intFilenumber
               muPreserved.blnCulculateComplete = True
               muPreserved.intisPrimeFlag = -1 * (lngNowProgress) - 1 '値を１か０に戻す
               If muPreserved.intisPrimeFlag = 1 Then      '素数だった場合、ユーザーに伝える
                       work = MsgBox("反例発見！！！", vbOKOnly, "Collatz's Anti-Exp＠ｈｏｍｅ")
               End If
               Kill App.Path & "\" & "CulculateData"
               Call Connect
               Exit Sub
            Else
               Close #intFilenumber
            End If
Exit Sub
Errorhandler:
         Resume Next
End Sub

Private Function Calculate()        'Ｃ言語のＥＸＥを用いて計算を行う関数
Dim intFilenumber As Integer
Dim strexist As String
    
        Label3.Caption = "2^" & CLng(muPreserved.strMustCulculate) & "-1を判定中"
        Label3.Left = (Form1.Width - Label3.Width) / 2
        Label4.Caption = muPreserved.intWorkCounts & "回目のワークです：データ取得時の順位　" & muPreserved.intPrize & "位"
        Label4.FontSize = Form1.Height * (72 / 1440) / 22
        Label4.Left = 0
        Label4.Top = 0
        intFilenumber = FreeFile
        strexist = Dir(App.Path & "\" & "EndTeller")            '終了する事を連絡するEndTellerのファイルがすでに存在している場合削除しておく
        If strexist = "EndTeller" Then
            Kill App.Path & "\" & "Endteller"
        End If
        ChDir App.Path
            Call FileCodeOut("CulculateData", xORnumber)
            lngProcessID = Shell(App.Path & "\" & "collatz.exe" & " " & CLng(muPreserved.strMustCulculate) & " " & App.Path, vbHide)       'Ｃ言語のソフトを起動する
            Timer1.Enabled = True                 'Ｃ言語のソフトの終了が書き込まれるファイルの値を確かめるタイマーの起動
            mblnEndFlag = True
End Function

Private Function Connect()        'サーバーへの接続を行う関数
Dim sngStartTime As Single         '接続タイムアウト判断のための時刻保持用
Dim work As Integer
Dim work2 As Long
        
        On Error GoTo errorhundler
        Timer1.Enabled = False
        If muPreserved.blnAutoConnectConsent = False Then        '自動接続を許可していない場合
                 work = MsgBox("サーバへ接続をしますがよろしいでしょうか？", vbOKCancel, "Collatz's Anti-Exp＠ｈｏｍｅ")
                 If work = vbCancel Then
                        mblnEndFlag = True
                        Label3.Caption = "待機中"
                        Label3.Left = (Form1.Width - Label3.Width) / 2
                        mnuExecute.Visible = True
                        Exit Function
                 End If
        End If
        Winsock1.RemoteHost = ChangeAddres(SpaceTrim(muPreserved.strAddres))    'サーバのコンピュータ名設定
        If Winsock1.RemoteHost = "" Then         '対応するアドレスが見つからなかった場合
                        mblnEndFlag = True
                        Label3.Caption = "サーバに異常有り・待機中"
                        Label3.Left = (Form1.Width - Label3.Width) / 2
                        mnuExecute.Visible = True
        End If
        Label4.Caption = ""
        Label3.Caption = "サーバ接続中"
        Label3.Left = (Form1.Width - Label3.Width) / 2
        Winsock1.Close
            Winsock1.Connect
            sngStartTime = Timer
            Do                        '接続処理が完了するまで待機する
               DoEvents
               If Timer - sngStartTime > 30 Then         '30秒たっても接続が確立できない場合は、異常有りとして終了する
                  GoTo errorhundler
                  Exit Do
               End If
               If mblnConnectionFlag = True Then
                  mblnConnectionFlag = False
                  Exit Do
               End If
            Loop
            DoEvents
            Label3.Caption = "データ送信中"
            Label3.Left = (Form1.Width - Label3.Width) / 2
            work2 = muPreserved.lngWorkingSecond + (Timer - lngProgramStartTime)                'ここでも経過秒数を代入しておく
            Timer3.Enabled = True            '一分経過後でも応答がなかった場合このタイマーによって強制終了する
            If muPreserved.blnSpecSendConsent = True Then    '最初の接続かつスペックの送信を許可している場合
                        Winsock1.SendData InCode(SpaceTrim(muPreserved.strName) & "!" & SpaceTrim(muPreserved.strMustCulculate) & "!" & muPreserved.intFilenumber & "!" & muPreserved.intisPrimeFlag & "!" & SpaceTrim(muPreserved.strMassageTo) & "!" & GetMyCpuClock() & "!" & getCpuType & "!" & getMemolySize() & "!" & work2, xORnumber) & "#"
            Else
                        Winsock1.SendData InCode(SpaceTrim(muPreserved.strName) & "!" & SpaceTrim(muPreserved.strMustCulculate) & "!" & muPreserved.intFilenumber & "!" & muPreserved.intisPrimeFlag & "!" & SpaceTrim(muPreserved.strMassageTo) & "!" & "0" & "!" & "secret" & "!" & "0" & "!" & work2, xORnumber) & "#"
            End If

Exit Function
errorhundler:
    If mintFoultCount >= 5 Then                    'すでに５回接続を失敗してしまっている場合
        work = MsgBox("サーバーが応答しないため終了します", vbOKOnly, "Collatz's Anti-Exp＠ｈｏｍｅ")
        Call EndWorking
    Else
        Label3.Caption = "接続失敗・待機中"
        Label3.Left = (Form1.Width - Label3.Width) / 2
        Timer2.Enabled = True                      '一分後に再び接続を試みるタイマーを起動する
        mintFoultCount = mintFoultCount + 1
        mblnEndFlag = True
    End If
End Function

Private Sub Picture1_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
      If (Button And vbMiddleButton) = vbMiddleButton Then
        Select Case X \ Screen.TwipsPerPixelX
        Case WM_LBUTTONDOWN
            Call ShowForm
        Case WM_RBUTTONDOWN
'             Me.PopupMenu mnuAllowTime, vbPopupMenuRightButton
        End Select
    End If
End Sub

Private Sub Form_Resize()                    '最小化ボタンが押された場合、ツールバーに格納する
    If blnFormVisibleFlag = True Then
        blnFormVisibleFlag = False
        Form1.Visible = False
        lRetVal = Shell_NotifyIcon(NIM_ADD, nidSysInfo)
    End If
End Sub

Public Function ShowForm()           '初期設定を行いながらフォームを表示する
Dim work As Integer
        
        Form1.Visible = True
        Form1.WindowState = vbNormal
        Form1.Caption = "Collatz's Anti-Exp@Ｈｏｍｅ"
        Form1.Height = Screen.Height * (7 / 10)
        Form1.Width = Screen.Width * (7 / 10)
        Form1.BackColor = vbScrollBars
        Label1.FontSize = Form1.Height * (72 / 1440) / 14                 'Ｔｗｉｐをポイントに変換してから文字の大きさを調整
        Label1.Left = (Form1.Width - Label1.Width) / 2
        Label1.Top = Form1.Height * (2 / 10)
        Label5.FontSize = Form1.Height * (72 / 1440) / 14
        Label5.Top = Label1.Top - Label5.Height
        Label5.Left = (Form1.Width - Label1.Width) / 2
        Label2.FontSize = Form1.Height * (72 / 1440) / 10
        Label2.Left = (Form1.Width - Label2.Width) / 2
        Label2.Top = Form1.Height * (3 / 10)
        Label3.FontSize = Form1.Height * (72 / 1440) / 10
        Label3.Top = Form1.Height * (6 / 10)
        Label3.Left = (Form1.Width - Label3.Width) / 2
        work = SetWindowPos(mHwnd, HWND_TOPMOST, 0, 0, 0, _
                0, SWP_SHOWWINDOW Or SWP_NOMOVE Or SWP_NOSIZE)  'ウィンドウを常に手前に表示
        lRetVal = Shell_NotifyIcon(NIM_DELETE, nidSysInfo)
        blnFormVisibleFlag = True      'フォームが表示されている事を示す
End Function
Public Function Savedata()             'レジストリーファイルに現在の状態を書き込む関数
Dim intFilenumber As Integer
    
    Call FileCodeOut("Registry", xORnumber)
    intFilenumber = FreeFile
    Open App.Path & "\" & "Registry" For Random As intFilenumber Len = Len(muPreserved)         'この時点での情報を書き込む
        Put #intFilenumber, 1, muPreserved
    Close #intFilenumber
    Call FileCodeIn("Registry", xORnumber)               'ファイルを暗号化しておく
End Function

Public Function LoadData()              'レジストリーファイルからデータを読み込む関数
Dim intFilenumber As Integer
Dim strexist As String
Dim work As Integer

On Error GoTo errorhundler
    Call FileCodeOut("Registry", xORnumber)               'ファイルを復号化する
    strexist = Dir(App.Path & "\" & "Registry")       'ファイルが存在する場合
        If strexist = "Registry" Then
            intFilenumber = FreeFile
            Open App.Path & "\" & "Registry" For Random As intFilenumber Len = Len(muPreserved)
                Get #intFilenumber, 1, muPreserved              'レジストリーファイルから情報を取得
            Close #intFilenumber
        End If
    Call FileCodeIn("Registry", xORnumber)         'ファイルを暗号化する
    
Exit Function
errorhundler:
    work = MsgBox("エラーが発生しました。初期設定をやりなおして下さい", vbOKOnly, "Collatz's Anti-Exp＠ｈｏｍｅ")
    mblnEndFlag = True
    Call mnuPrepare_Click
End Function

Private Function EndWorking()            '終了処理を行う関数
Dim intFilenumber As Integer
Dim strexist As String
Dim ProcesshWnd As Long
Dim ExitCode As Long
Dim ret As Long

    strexist = Dir(App.Path & "\" & "CulculateData")
    If strexist = "CulculateData" Then            'ＣｕｌｃｕｌａｔｅＤａｔａが存在する。つまり、Ｃ言語のアプリが計算をまだ完了させず、起動している場合
            intFilenumber = FreeFile
            Open App.Path & "\" & "EndTeller" For Binary As intFilenumber
            Close intFilenumber
    End If
    muPreserved.lngWorkingSecond = muPreserved.lngWorkingSecond + (Timer - lngProgramStartTime)         '今回のプログラム起動時間を総起動時間に加算する
    Call Savedata
    If lngProcessID <> 0 Then       '一度でもＳｈｅｌｌ関数を用いていた場合
            ProcesshWnd = OpenProcess(PROCESS_QUERY_INFORMATION, 1, lngProcessID)      'プロセスハンドルを得る
            If ProcesshWnd <> 0 Then       'プロセスハンドルが得られた場合
                Do                                                       'プロセスが起動中の場合は待機
                    Call GetExitCodeProcess(ProcesshWnd, ExitCode)
                    DoEvents
                Loop While (ExitCode = STILL_ACTIVE)
            End If
            Call CloseHandle(ProcesshWnd)   'プロセスハンドルを解放する
            Call FileCodeIn("CulculateData", xORnumber)    'ファイルを暗号化する
    End If
    If blnFormVisibleFlag = False Then       'もしタスクトレイにアイコンがある場合取り除く
            lRetVal = Shell_NotifyIcon(NIM_DELETE, nidSysInfo)
    End If
    End
End Function

Private Function SpaceTrim(Data As String) As String        'ランダムアクセスで書き込んだ時に末尾に埋められたスペースを取り除く関数
     SpaceTrim = Trim(Replace(Data, Chr(0), ""))
End Function

Private Function getMemolySize() As Integer
On Error Resume Next
    ChDir App.Path
    getMemolySize = GetMemDat(1) / 1024 / 1024
End Function

Private Function GetMyCpuClock() As Double           'ＣＰＵのクロック情報を返す
On Error Resume Next
   '  MHz単位にするため、1000000(=1M)で割る
    ChDir App.Path
    GetMyCpuClock = Format(getCpuClock() / 1000000, "0.00")
End Function

Private Function getCpuType() As String
Dim ret As Long
Dim str As String

On Error Resume Next
    ChDir App.Path
    ' DLL内の関数を呼び出す
    ret = GetProcessorType()

    Select Case ret
    Case 0
         str = "INTEL 386"
    Case 1
         str = "INTEL 486"
    Case 2
         str = "INTEL PENTIUM"
    Case 3
         str = "MIPS R4000"
    Case 4
         str = "ALPHA 21064"
    Case Else
         str = "Unknown"
    End Select
    getCpuType = str

End Function

Private Function ChangeTime(ByVal Second As Long) As String             '秒数を渡すと、ｘ時間、ｘ分、ｘ秒に直して、文字列として返す関数
Dim Hour As Long
Dim Minute As Long
Dim work As Long
Dim str As String

            Hour = Second \ 3600
            work = Second Mod 3600
            Minute = work \ 60
            Second = work Mod 60
            If Hour <> 0 Then
                  str = Hour & "時間"
            End If
            If Minute <> 0 Then
                  str = str & Minute & "分"
            End If
            If Second <> 0 Then
                  str = str & Second & "秒"
            End If
            ChangeTime = str
End Function

Private Function InCode(ByVal str As String, xORnumber As Integer) As String           '文字列を渡すと暗号化された文字列を返す（Ｘｏｒ結合で結合する値が必要）
Dim bytArray() As Byte
Dim i As Long
Dim xORnumber2 As Integer
Dim xORnumber3 As Integer

            xORnumber2 = xORnumber \ 2 + 1        '２度目のXor結合で利用する値
            xORnumber3 = xORnumber \ 3 + 2
            ReDim bytArray(lstrlen(str))
            Call MoveMemory(bytArray(0), ByVal str, lstrlen(str))
            str = ""
            For i = 0 To UBound(bytArray) Step 2
                bytArray(i) = CByte(bytArray(i) Xor xORnumber)
            Next i
            For i = 1 To UBound(bytArray) Step 2
                bytArray(i) = CByte(bytArray(i) Xor xORnumber2)
            Next i
            For i = 0 To UBound(bytArray)
                bytArray(i) = CByte(bytArray(i) Xor xORnumber3)
                str = str & "," & bytArray(i)
            Next i
                str = Right(str, Len(str) - 1)
                InCode = str
End Function

Private Function OutCode(ByVal str As String, xORnumber As Integer)                 '暗号化された文字列を渡すと復号化して返す（Ｘｏｒ結合で結合した値が必要）
Dim bytArray() As Byte
Dim valsplited As Variant
Dim i As Long
Dim xORnumber2 As Integer
Dim xORnumber3 As Integer

            xORnumber2 = xORnumber \ 2 + 1        '２度目のXor結合で利用する値
            xORnumber3 = xORnumber \ 3 + 2
            valsplited = Split(str, ",")
            ReDim bytArray(UBound(valsplited))
            For i = 0 To UBound(valsplited)
                bytArray(i) = CByte(valsplited(i))
                bytArray(i) = CByte(bytArray(i) Xor xORnumber3)
            Next i
            For i = 0 To UBound(valsplited) Step 2
                bytArray(i) = CByte(bytArray(i) Xor xORnumber)
            Next i
            For i = 1 To UBound(valsplited) Step 2
                bytArray(i) = CByte(bytArray(i) Xor xORnumber2)
            Next i
            OutCode = StrConv(bytArray, vbUnicode)
End Function

Public Function FileCodeIn(FileNum As String, xORnumber As Integer)              '与えたファイルの暗号化をする　※　ファイル名に拡張子がなく、また、ファイル名のみのもの、＋が付いていないもの（Ｘｏｒ結合で結合する値が必要）
Dim Dat() As Byte                'ファイルのデータ
Dim i As Long, Lf As Long      'カウンタとファイルサイズ
Dim intFilenumber As Integer                'ファイルナンバー
Dim strexist As String
Dim xORnumber2 As Integer
Dim xORnumber3 As Integer

On Error GoTo errorhundler
        strexist = Dir(App.Path & "\" & FileNum)       '暗号化されていないファイルが存在する場合
        If strexist = FileNum Then
            xORnumber2 = xORnumber \ 2 + 1        '２度目のXor結合で利用する値
            xORnumber3 = xORnumber \ 3 + 2
            '暗号化するファイルのデータを読み込む
            intFilenumber = FreeFile
            Open App.Path & "\" & FileNum For Binary As intFilenumber
                   Lf = LOF(intFilenumber)
                   ReDim Dat(Lf)
                   Get #intFilenumber, 1, Dat
            Close intFilenumber
            
            '暗号化処理
            For i = 0 To Lf Step 2
                Dat(i) = CByte(Dat(i) Xor xORnumber)
            Next i
            For i = 1 To Lf Step 2
                Dat(i) = CByte(Dat(i) Xor xORnumber2)
            Next i
            For i = 1 To Lf
                Dat(i) = CByte(Dat(i) Xor xORnumber3)
            Next i
               
            '暗号化したデータをファイルに書き込む
            intFilenumber = FreeFile
            Open App.Path & "\" & FileNum & "＋" For Binary As intFilenumber
            Put #intFilenumber, 1, Dat
            Close intFilenumber
            
            Kill App.Path & "\" & FileNum        '元のファイルを削除しておく
        End If
Exit Function
errorhundler:
Resume
                    
End Function

Public Function FileCodeOut(FileNum As String, xORnumber As Integer)              '与えたファイルの復号化をする　※ファイル名に拡張子がなく、また、ファイル名のみ（Ｘｏｒ結合で結合した値が必要）
Dim Dat() As Byte                'ファイルのデータ
Dim i As Long, Lf As Long      'カウンタとファイルサイズ
Dim intFilenumber As Integer                'ファイルナンバー
Dim strexist As String
Dim xORnumber2 As Integer
Dim xORnumber3 As Integer
       
       strexist = Dir(App.Path & "\" & FileNum & "＋")        'ファイルが存在する場合
       If strexist = FileNum & "＋" Then            'ファイル名に暗号化されている事を示す＋が付いているものが存在する場合
            xORnumber2 = xORnumber \ 2 + 1        '２度目のXor結合で利用する値
            xORnumber3 = xORnumber \ 3 + 2
            '復号化するファイルのデータを読み込む
            intFilenumber = FreeFile
            Open App.Path & "\" & FileNum & "＋" For Binary As intFilenumber
                   Lf = LOF(intFilenumber)
                   ReDim Dat(Lf)
                   Get #intFilenumber, 1, Dat
            Close intFilenumber
            
            '復号化処理
            For i = 1 To Lf
                Dat(i) = CByte(Dat(i) Xor xORnumber3)
            Next i
            For i = 0 To Lf Step 2
                Dat(i) = CByte(Dat(i) Xor xORnumber)
            Next i
            For i = 1 To Lf Step 2
                Dat(i) = CByte(Dat(i) Xor xORnumber2)
            Next i
            
            '復号化したデータをファイルに書き込む
            intFilenumber = FreeFile
            Open App.Path & "\" & FileNum For Binary As intFilenumber
            Put #intFilenumber, 1, Dat
            Close intFilenumber
                       
            Kill App.Path & "\" & FileNum & "＋"                  '元のファイルを削除しておく
       End If
End Function

Private Function ChangePriorityC(PriorityClass As Long)        'Ｃ言語のＥＸＥが起動されている場合、優先度を変更する関数　引数は優先度を示すクラス
Dim ProcesshWnd As Long
            
    If lngProcessID <> 0 Then       '一度でもＳｈｅｌｌ関数を用いていた場合
            ProcesshWnd = OpenProcess(PROCESS_QUERY_INFORMATION, 1, lngProcessID)      'プロセスハンドルを得る
            If ProcesshWnd <> 0 Then       'プロセスハンドルが得られた場合
                    Call SetPriorityClass(ProcesshWnd, PriorityClass)           'プロセスの優先度を変更する
            End If
            Call CloseHandle(ProcesshWnd)   'プロセスハンドルを解放する
    End If
End Function

Private Function ChangeAddres(Addres As String) As String         '与えられたドメインをＩＰアドレスに変換する関数　引数はドメイン　戻り値はＩＰアドレス ＩＰアドレスを渡した場合はそのまま返す
Dim ChangeClass As ws            'クラスを実装する変数
Dim work As String
    On Error Resume Next
        Set ChangeClass = New ws         'クラスをロードする
        work = ChangeClass.GetIP(Addres) 'ホスト名:xxxのIPアドレスを表示
        Set ChangeClass = Nothing        'クラスを解放する
        ChangeAddres = work
End Function








[module1.bas]



Option Explicit

Public Const xORnumber = 173       '暗号化において結合する値

Public Type Preserved
    strName As String * 100                'ユーザー名保持用
    strAddres As String * 100              'サーバーのアドレス保持用
    strMustCulculate As String * 100            '計算すべき値を保持
    intFilenumber As Integer              'サーバが保存しているクライアントデータファイルのどこに自分のデータが保持されているか
    lngNowProgress As Long             '現在どこまで計算が終了しているか
    intisPrimeFlag As Integer              '判定結果が素数であったか、素数ではなかったか 1=真　0=偽
    intWorkCounts As Integer               'このクライアントがどれだけのワーク数をこなしたか
    strMassageTo As String * 1000               'サーバーへのメッセージ
    blnAutoConnectConsent As Boolean       '自動接続を認めるかどうか
    blnSpecSendConsent As Boolean         'パソコンのスペック情報を送信してもよいか
    blnCulculateComplete As Boolean        '計算は終了しているか
    intPrize As Integer                    'サーバーへ接続を行った時点での順位
    lngWorkingSecond As Long             'プログラムの総起動時間を秒数で保持
    blnAutoStartFlag As Boolean           'プログラムが起動した後、自動で処理を始めてよいか
End Type

Declare Function lstrlen Lib "KERNEL32" Alias "lstrlenA" (ByVal lpString As String) As Long    ' lstrlen()関数　文字列のバイト数を得るＡＰＩの宣言
Declare Sub MoveMemory Lib "KERNEL32" Alias "RtlMoveMemory" (pDest As Any, pSource As Any, ByVal ByteLen As Long)      ' メモリ操作関数　文字列をバイト配列に代入する


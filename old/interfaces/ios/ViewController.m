#import "ViewController.h"
#include "nga.h"
#import "SolarizedColors.h"
#import "NgaBridge.h"

@interface ViewController ()
@end
@implementation ViewController

#pragma mark - View Functionality

UITextView *Input;
UITextView *Output;
UIBarButtonItem *Go;
UIBarButtonItem *Clear;
UIBarButtonItem *Reload;
UIBarButtonItem *Save;
UIBarButtonItem *Ref;
UIBarButtonItem *Legal;
UIBarButtonItem *flexibleSpace;
UIToolbar *keyboardAccessory;
UIToolbar *ButtonBar;
UIScrollView *scrollView;
SolarizedColors *Solarized;

NgaBridge *nga;


/*
 *  +---------------+---------------+
 *  | input area    | toolbar       |
 *  |               +---------------+
 *  |               | output area   |
 *  |               |               |
 *  |               |               |
 *  +---------------+---------------+
 *
 */

- (void)viewWillAppear:(BOOL)animated {
    [super viewWillAppear:animated];
    [self resetTheme];
}


- (void)resetTheme {
    [Input setBackgroundColor:[Solarized inputBackground]];
    [Input setTextColor:[Solarized inputText]];
    [Output setBackgroundColor:[Solarized outputBackground]];
    [Output setTextColor:[Solarized outputText]];
    [Go setTintColor:[Solarized buttonText]];
    [Clear setTintColor:[Solarized buttonText]];
    [Save setTintColor:[Solarized buttonText]];
    [Reload setTintColor:[Solarized buttonText]];
    if (![[NSUserDefaults standardUserDefaults] boolForKey:@"wantsDark"])
        Input.keyboardAppearance = UIKeyboardAppearanceDark;
    else
        Input.keyboardAppearance = UIKeyboardAppearanceLight;
    ButtonBar.translucent = NO;
    ButtonBar.barTintColor = [Solarized base0];
    ButtonBar.tintColor = [Solarized base03];
    if ([[NSUserDefaults standardUserDefaults] boolForKey:@"hidebuttons"] == NO) {
        ButtonBar.items = @[
                            Save,
                            flexibleSpace,
                            Reload,
                            flexibleSpace,
                            Ref,
                            flexibleSpace,
                            Legal,
                            flexibleSpace,
                            Clear,
                            flexibleSpace,
                            Go,
                            ];
    } else {
        ButtonBar.items = @[
                            Save,
                            Reload,
                            flexibleSpace,
                            Clear,
                            Go,
                            ];
    }

}

- (void)loadView {
/*
    for (NSString* family in [UIFont familyNames])
    {
        NSLog(@"%@", family);
        
        for (NSString* name in [UIFont fontNamesForFamilyName: family])
        {
            NSLog(@"  %@", name);
        }
    }
*/
    nga = [[NgaBridge alloc] init];
    Solarized = [SolarizedColors new];
    CGRect rect = [UIScreen mainScreen].bounds;
    self.view = [[UIView alloc] initWithFrame:rect];
    self.view.backgroundColor = [Solarized base0];
    
    flexibleSpace = [[UIBarButtonItem alloc] initWithBarButtonSystemItem:UIBarButtonSystemItemFlexibleSpace target:nil action:nil];

    
    /* Input */
    Input = [[UITextView alloc] initWithFrame:CGRectMake(0, 0, rect.size.width / 2, rect.size.height)  textContainer:Nil];
    [Input setFont:[UIFont fontWithName:@"GoMono" size:14]];
    [Input setDelegate:self];
    
    
    /* Output */
    Output = [[UITextView alloc] initWithFrame:CGRectMake(rect.size.width / 2, 50, rect.size.width / 2, rect.size.height - 50)  textContainer:Nil];
    [Output setFont:[UIFont fontWithName:@"GoMono" size:14]];
    [Output setDelegate:self];
    
    
    /* Toolbar */
    ButtonBar = [[UIToolbar alloc] initWithFrame:CGRectMake(rect.size.width / 2, 0, rect.size.width / 2, 50)];
    ButtonBar.backgroundColor = [Solarized base0];
    
    Go = [[UIBarButtonItem alloc] initWithTitle:@"Go" style:UIBarButtonItemStylePlain target:self action:@selector(buttonClicked:)];
    [Go setTitleTextAttributes:@{NSFontAttributeName: Input.font} forState:UIControlStateNormal];

    Clear = [[UIBarButtonItem alloc] initWithTitle:@"Clear" style:UIBarButtonItemStylePlain target:self action:@selector(buttonClicked:)];
    [Clear setTitleTextAttributes:@{NSFontAttributeName: Input.font} forState:UIControlStateNormal];

    Ref = [[UIBarButtonItem alloc] initWithTitle:@"Glossary" style:UIBarButtonItemStylePlain target:self action:@selector(buttonClicked:)];
    [Ref setTitleTextAttributes:@{NSFontAttributeName: Input.font} forState:UIControlStateNormal];

    Legal = [[UIBarButtonItem alloc] initWithTitle:@"(C)" style:UIBarButtonItemStylePlain target:self action:@selector(buttonClicked:)];
    [Legal setTitleTextAttributes:@{NSFontAttributeName: Input.font} forState:UIControlStateNormal];

    Save = [[UIBarButtonItem alloc] initWithTitle:@"Save" style:UIBarButtonItemStylePlain target:self action:@selector(buttonClicked:)];
    [Save setTitleTextAttributes:@{NSFontAttributeName: Input.font} forState:UIControlStateNormal];

    Reload = [[UIBarButtonItem alloc] initWithTitle:@"Load" style:UIBarButtonItemStylePlain target:self action:@selector(buttonClicked:)];
    [Reload setTitleTextAttributes:@{NSFontAttributeName: Input.font} forState:UIControlStateNormal];

    [self resetTheme];
    
    if ([[NSUserDefaults standardUserDefaults] boolForKey:@"hidebuttons"] == NO) {
    ButtonBar.items = @[
                        Save,
                        flexibleSpace,
                        Reload,
                        flexibleSpace,
                        Ref,
                        flexibleSpace,
                        Legal,
                        flexibleSpace,
                        Clear,
                        flexibleSpace,
                        Go,
                       ];
    } else {
        ButtonBar.items = @[
                            Save,
                            Reload,
                            flexibleSpace,
                            Clear,
                            Go,
                            ];
    }
    /* Add views to main view */
    [self.view addSubview:Input];
    [self.view addSubview:Output];
    [self.view addSubview:ButtonBar];

    keyboardAccessory = [[UIToolbar alloc] initWithFrame:CGRectMake(0.0f,
                                                                     0.0f,
                                                                     self.view.window.frame.size.width,
                                                                     44.0f)];

    /* This feels ugly */
    scrollView = [[UIScrollView alloc] init];
    scrollView.frame = keyboardAccessory.frame;
    scrollView.bounds = keyboardAccessory.bounds;
    scrollView.autoresizingMask = keyboardAccessory.autoresizingMask;
    scrollView.showsVerticalScrollIndicator = false;
    scrollView.showsHorizontalScrollIndicator = false;
    //scrollView.bounces = false;
    UIView *superView = keyboardAccessory.superview;
    [keyboardAccessory removeFromSuperview];
    keyboardAccessory.autoresizingMask = UIViewAutoresizingNone;
    keyboardAccessory.frame = CGRectMake(0, 0, self.view.frame.size.width, keyboardAccessory.frame.size.height);
    keyboardAccessory.bounds = keyboardAccessory.frame;
    scrollView.contentSize = keyboardAccessory.frame.size;
    [scrollView addSubview:keyboardAccessory];
    [superView addSubview:scrollView];
    
    keyboardAccessory.translucent = NO;
    keyboardAccessory.barTintColor = [Solarized base0];
    keyboardAccessory.tintColor = [Solarized base03];
    keyboardAccessory.items = @[
                      [self kbdButton:@"````"],
                      flexibleSpace,
                      [self kbdButton:@":"],
                      [self kbdButton:@";"],
                      [self kbdButton:@"#"],
                      [self kbdButton:@"&"],
                      [self kbdButton:@"$"],
                      [self kbdButton:@"'"],
                      [self kbdButton:@"?"],
                      flexibleSpace,
                      [self kbdButton:@"+"],
                      [self kbdButton:@"-"],
                      [self kbdButton:@"*"],
                      [self kbdButton:@"/"],
                      flexibleSpace,
                      [self kbdButton:@"["],
                      [self kbdButton:@"]"],
                      [self kbdButton:@"("],
                      [self kbdButton:@")"],
                      [self kbdButton:@"<"],
                      [self kbdButton:@">"],
                      [self kbdButton:@"{"],
                      [self kbdButton:@"}"],
                      flexibleSpace,
                      [self kbdButton:@"1"],
                      [self kbdButton:@"2"],
                      [self kbdButton:@"3"],
                      [self kbdButton:@"4"],
                      [self kbdButton:@"5"],
                      [self kbdButton:@"6"],
                      [self kbdButton:@"7"],
                      [self kbdButton:@"8"],
                      [self kbdButton:@"9"],
                      [self kbdButton:@"0"],
                      ];
    
    [scrollView sizeToFit];
    
    Input.inputAccessoryView = scrollView;
    
    /* For resizing the view when the keyboard is opened/closed */
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(toggleKeyboard:)
                                                 name:UIKeyboardDidShowNotification object:nil];
        
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(toggleKeyboard:)
                                                 name:UIKeyboardWillHideNotification object:nil];
    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(applicationEnteringForeground:)
                                                 name:UIApplicationWillEnterForegroundNotification
                                               object:nil];

}

- (IBAction)applicationEnteringForeground:(id)sender {
    [self resetTheme];
}

-(IBAction)barButtonAddText:(UIBarButtonItem*)sender
{
    if (Input.isFirstResponder)
    {
        NSInteger cursorPos = 0;
        if ([sender.title isEqualToString:@"tab"])
            [Input insertText:@"\t"];
        else if ([sender.title isEqualToString:@"←"]) {
            cursorPos = [Input selectedRange].location;
            if (cursorPos > 0)
                [Input setSelectedRange:NSMakeRange(cursorPos - 1, 0)];
        }
        else if ([sender.title isEqualToString:@"→"]) {
            cursorPos = [Input selectedRange].location;
            [Input setSelectedRange:NSMakeRange(cursorPos + 1, 0)];
        }
        else if ([sender.title isEqualToString:@"````"]) {
            [Input insertText:@"````\n"];
        }
        else
            [Input insertText:sender.title];
    }
}

- (UIBarButtonItem *)kbdButton:(NSString *)l {
    UIBarButtonItem *b = [[UIBarButtonItem alloc] initWithTitle:l
                                                          style:UIBarButtonItemStylePlain
                                                         target:self
                                                         action:@selector(barButtonAddText:)];
    
    [b setTitleTextAttributes:@{NSFontAttributeName: Input.font} forState:UIControlStateNormal];
    return b;
}

- (BOOL)textViewShouldBeginEditing:(UITextView *)textView {
    if (textView == Output)
        return NO;
    else
        return YES;
}

- (void)textViewDidChange:(UITextView *)textView {
    if (textView == Input) {
        [self cacheInput];
    }
}

- (IBAction) buttonClicked:(id)sender {
    if (sender == Go) {
        [self cacheInput];
        ngaPrepare();
        [self reloadImage];
        [nga closeAll];
        [self evaluateCodeBlocksInTokenSet:[self tokenize:[Input text]]];
        [Output setText:[NSString stringWithFormat:@"%@\nStack: %@\n------------\n",
                         [Output text], [self dumpStack]]];
    }
    if (sender == Clear) {
        [Output setText:@""];
    }
    if (sender == Reload) {
        [self loadSnapshot];
    }
    if (sender == Save) {
        [self saveSnapshot];
    }
    if (sender == Ref) {
        NSString *file = [[NSBundle mainBundle] pathForResource:@"Glossary" ofType:@"txt"];
        NSString *str = [NSString stringWithContentsOfFile:file
                                                  encoding:NSUTF8StringEncoding error:NULL];
        [Output setText:str];
    }
    if (sender == Legal) {
        NSString *file = [[NSBundle mainBundle] pathForResource:@"Licenses" ofType:@"txt"];
        NSString *str = [NSString stringWithContentsOfFile:file
                                                  encoding:NSUTF8StringEncoding error:NULL];
        [Output setText:str];
    }
}

- (void)viewWillTransitionToSize:(CGSize)size
       withTransitionCoordinator:(id<UIViewControllerTransitionCoordinator>)coordinator {
    [coordinator animateAlongsideTransition:^(id<UIViewControllerTransitionCoordinatorContext> context)
     {
         [self.view setFrame:CGRectMake(0, 0, size.width, size.height)];
         [Input setFrame:CGRectMake(0, 0, size.width / 2, size.height)];
         [Output setFrame:CGRectMake(size.width / 2, 50, size.width / 2, size.height - 50)];
         [ButtonBar setFrame:CGRectMake(size.width / 2, 0, size.width / 2, 50)];
         keyboardAccessory.frame = CGRectMake(0, 0, size.width, keyboardAccessory.frame.size.height);
         keyboardAccessory.bounds = keyboardAccessory.frame;
     } completion:^(id<UIViewControllerTransitionCoordinatorContext> context)
     {
         
     }];
    [super viewWillTransitionToSize:size withTransitionCoordinator:coordinator];
}

- (void)viewDidLoad {
    ngaPrepare();
    [self reloadImage];
    [self loadCachedInput];
    CGPoint bottomOffset = CGPointMake(0, Input.contentSize.height - Input.bounds.size.height);
    [Input setContentOffset:bottomOffset animated:YES];
}

- (void)viewWillDisappear:(BOOL)animated {
    [self cacheInput];
    [self resetTheme];
}

- (void)toggleKeyboard:(NSNotification *)note {
    NSDictionary *userInfo = note.userInfo;
    NSTimeInterval duration = [userInfo[UIKeyboardAnimationDurationUserInfoKey] doubleValue];
    UIViewAnimationCurve curve = [userInfo[UIKeyboardAnimationCurveUserInfoKey] integerValue];
    
    CGRect keyboardFrameEnd = [userInfo[UIKeyboardFrameEndUserInfoKey] CGRectValue];
    keyboardFrameEnd = [self.view convertRect:keyboardFrameEnd fromView:nil];
    
    [UIView animateWithDuration:duration delay:0 options:UIViewAnimationOptionBeginFromCurrentState | curve animations:^{
        self.view.frame = CGRectMake(0, 0, keyboardFrameEnd.size.width, keyboardFrameEnd.origin.y);
        
        [Input setFrame:CGRectMake(0, 0, self.view.frame.size.width / 2, self.view.frame.size.height)];
        [Output setFrame:CGRectMake(self.view.frame.size.width / 2, 50, self.view.frame.size.width / 2, self.view.frame.size.height - 50)];
    } completion:nil];
}

#pragma mark - Storage
- (void)cacheInput {
    [[NSUserDefaults standardUserDefaults] setObject:[NSString stringWithString:[Input text]]
                                              forKey:@"Input"];
}

- (void)loadCachedInput {
    if ([[[NSUserDefaults standardUserDefaults] stringForKey:@"Input"] length] == 0) {
        NSString *file = [[NSBundle mainBundle] pathForResource:@"Initial" ofType:@"md"];
        NSString *str = [NSString stringWithContentsOfFile:file
                                                  encoding:NSUTF8StringEncoding error:NULL];
        [Input setText:str];
    } else {
        [Input setText:[[NSUserDefaults standardUserDefaults] stringForKey:@"Input"]];
    }
}

- (void)saveSnapshot {
    [[NSUserDefaults standardUserDefaults] setObject:[NSString stringWithString:[Input text]]
                                              forKey:@"UserCode"];
}

- (void)loadSnapshot {
    [Input setText:[[NSUserDefaults standardUserDefaults] stringForKey:@"UserCode"]];
}

#pragma mark - Objective-C to Rx

- (void)reloadImage {
    for (CELL i = 0; i < ngaImageCells; i++)
        memory[i] = ngaImage[i];
    [self loadExtensions];
}

- (NSArray *)tokenize:(NSString *)source {
    NSArray *tokens = [source componentsSeparatedByCharactersInSet:
                       [NSCharacterSet characterSetWithCharactersInString:@" \n\r\t"]];
    return tokens;
}

- (void)evaluateCodeBlocksInTokenSet:(NSArray *)tokens {
    BOOL InBlock = FALSE;
    for (id token in tokens) {
        if (InBlock == TRUE && ![token isEqualToString:@"````"])
            [Output setText:[NSString stringWithFormat:@"%@%@", [Output text], [nga evaluateToken:token]]];
        else if (InBlock == FALSE && [token isEqualToString:@"````"])
            InBlock = TRUE;
        else if (InBlock == TRUE && [token isEqualToString:@"````"])
            InBlock = FALSE;
    }
}

- (NSString *)dumpStack {
    NSMutableString *stack = [[NSMutableString alloc] init];
    for (id value in [nga stackValues])
        [stack appendString:[NSString stringWithFormat:@"%ld ", (long)[value integerValue]]];
    return stack;
}

#pragma mark - default images


- (void)loadExtensions {
    NSString *iOSExtensions = @"````\n" \
                              @":file:open   #1 `1100 ;\n" \
                              @":file:close  #2 `1100 ;\n" \
                              @":file:read   #3 `1100 ;\n" \
                              @":file:write  #4 `1100 ;\n" \
                              @":file:pos    #5 `1100 ;\n" \
                              @":file:seek   #6 `1100 ;\n" \
                              @":file:length #7 `1100 ;\n" \
                              @":file:delete #8 `1100 ;\n" \
                              @":file:count-files    #9  `1100 ;\n" \
                              @":file:name-for-index #10 `1100 str:temp ;\n" \
                              @"````\n";
    [self evaluateCodeBlocksInTokenSet:[self tokenize:iOSExtensions]];
}


/* retro 2016.11 */
CELL ngaImageCells = 4691;
CELL ngaImage[] = { 1793,-1,4673,4690,201611,0,10,1,10,2,10,3,10,4,10,5,10,6,10,7,10,8,10,9,10,10,10,11,10,12,10,13,10,14,10,15,10,16,10,17,10,18,10,19,10,20,10,21,10,22,10,23,10,24,10,25,10,26,10,2049,10,67502597,10,2049,61,2049,61,10,68223234,1,2575,85000450,1,656912,2049,68,25,459011,74,524546,74,302256641,1,10,168756239,17043713,1,1,2577,134284549,63,2049,84,85263883,2049,85,302056966,1,25,1793,89,33620739,0,10,2049,63,2049,79,524548,79,590092,101,25,524546,79,134284289,-1,89,100860677,10,421,403,268505089,121,120,135205121,121,10,9,10,101384453,0,9,10,134287105,3,71,659457,3,524559,134,10,2049,68,25,2049,134,1793,142,2049,142,134283523,0,134,10,0,659201,155,524545,25,139,168820993,0,155,2049,156,25,134283523,7,139,2049,134,10,8,10,524545,59,139,2049,134,10,2049,156,134283521,175,173,122,10,2049,156,134283521,139,173,122,10,8,10,659713,0,659713,1,659713,2,659713,3,17108737,3,2,524559,134,2049,134,2049,134,2049,149,168820998,2,719,1471,167841793,218,5,17826049,0,218,2,15,25,524546,203,134287105,219,104,2305,220,459023,228,134287361,219,223,659201,218,1,1000,659969,48,318836481,244,10,10,2049,68,25,2049,245,2049,247,17826065,244,251,7,17826049,1,243,0,251793409,244,1641217,45,268501251,-1,243,659713,1,2049,262,2049,251,17760515,244,243,660239,0,112,114,101,102,105,120,58,59,0,285278479,284,7,2576,2049,293,524545,284,238,17826050,283,0,2572,2049,275,2049,164,10,524559,164,10,17760513,181,3,205,8,251727617,3,2,2049,199,268501264,-1,155,10,2049,238,2049,199,524559,164,10,285282049,3,3,16846593,155,-1,155,134283536,7,139,16846593,3,0,134,8,524545,19,139,659201,3,524545,25,139,17043201,3,7,2049,139,2049,134,268505092,155,2049,156,25,656131,659201,3,524545,7,139,2049,134,524545,19,139,10,2049,156,25,134283523,55,139,10,2049,156,25,134283523,15,139,10,2049,156,25,134283523,17,139,10,1793,5,10,524546,199,134284303,201,2063,10,1471,1642241,283,285282049,412,1,524548,406,10,134287105,218,406,10,134287105,412,238,16845825,0,421,403,2049,122,10,17826050,412,297,8,134283521,413,425,122,10,0,9,188,100,117,112,0,444,11,188,100,114,111,112,0,451,13,188,115,119,97,112,0,459,21,188,99,97,108,108,0,467,27,188,101,113,63,0,475,29,188,45,101,113,63,0,482,31,188,108,116,63,0,490,33,188,103,116,63,0,497,35,188,102,101,116,99,104,0,504,37,188,115,116,111,114,101,0,513,39,188,43,0,522,41,188,45,0,527,43,188,42,0,532,45,188,47,109,111,100,0,537,47,188,97,110,100,0,545,49,188,111,114,0,552,51,188,120,111,114,0,558,53,188,115,104,105,102,116,0,565,389,195,112,117,115,104,0,574,396,195,112,111,112,0,582,382,195,48,59,0,589,68,181,102,101,116,99,104,45,110,101,120,116,0,595,71,181,115,116,111,114,101,45,110,101,120,116,0,609,275,181,115,116,114,58,116,111,45,110,117,109,98,101,114,0,623,104,181,115,116,114,58,101,113,63,0,640,79,181,115,116,114,58,108,101,110,103,116,104,0,651,122,181,99,104,111,111,115,101,0,665,128,181,105,102,0,675,130,181,45,105,102,0,681,155,164,67,111,109,112,105,108,101,114,0,688,3,164,72,101,97,112,0,700,134,181,44,0,708,149,181,115,44,0,713,158,195,59,0,719,335,195,91,0,724,355,195,93,0,729,2,164,68,105,99,116,105,111,110,97,114,121,0,734,197,181,100,58,108,105,110,107,0,748,199,181,100,58,120,116,0,758,201,181,100,58,99,108,97,115,115,0,766,203,181,100,58,110,97,109,101,0,777,181,181,99,108,97,115,115,58,119,111,114,100,0,787,195,181,99,108,97,115,115,58,109,97,99,114,111,0,801,164,181,99,108,97,115,115,58,100,97,116,97,0,816,205,181,100,58,97,100,100,45,104,101,97,100,101,114,0,830,306,195,112,114,101,102,105,120,58,35,0,846,314,195,112,114,101,102,105,120,58,58,0,858,328,195,112,114,101,102,105,120,58,38,0,870,311,195,112,114,101,102,105,120,58,36,0,882,371,195,114,101,112,101,97,116,0,894,373,195,97,103,97,105,110,0,904,435,181,105,110,116,101,114,112,114,101,116,0,913,238,181,100,58,108,111,111,107,117,112,0,926,188,181,99,108,97,115,115,58,112,114,105,109,105,116,105,118,101,0,938,4,164,86,101,114,115,105,111,110,0,957,403,181,101,114,114,58,110,111,116,102,111,117,110,100,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,59,0,116,99,0,80,65,67,69,0,103,0,0,0,0,101,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,968,1543,181,69,79,77,0,1,524287,10,1536,1557,181,83,84,82,73,78,71,83,0,2049,1543,1,12,1,128,19,18,10,1546,1578,195,112,114,101,102,105,120,58,40,0,3,10,1566,1590,181,100,58,108,97,115,116,0,1,2,15,10,1580,1608,181,100,58,108,97,115,116,60,120,116,62,0,2049,1590,2049,199,15,10,1594,1631,181,100,58,108,97,115,116,60,99,108,97,115,115,62,0,2049,1590,2049,201,15,10,1614,1653,181,100,58,108,97,115,116,60,110,97,109,101,62,0,2049,1590,2049,203,10,1637,1669,181,114,101,99,108,97,115,115,0,2049,1590,2049,201,16,10,1658,1688,181,105,109,109,101,100,105,97,116,101,0,1,195,2049,1669,10,1675,1701,181,100,97,116,97,0,1,164,2049,1669,10,1693,1714,181,104,101,114,101,0,1,3,15,10,1706,1733,181,99,111,109,112,105,108,101,58,108,105,116,0,1,1,2049,134,2049,134,10,1718,1756,181,99,111,109,112,105,108,101,58,106,117,109,112,0,2049,1733,1,7,2049,134,10,1740,1779,181,99,111,109,112,105,108,101,58,99,97,108,108,0,2049,1733,1,8,2049,134,10,1763,1801,181,99,111,109,112,105,108,101,58,114,101,116,0,1,10,2049,134,10,1786,1818,195,112,114,101,102,105,120,58,96,0,1,155,15,1,1829,7,2049,275,2049,134,10,1,1824,1,1836,7,3,10,1,1834,2049,122,10,1806,1853,181,100,58,99,114,101,97,116,101,0,1,164,1,0,2049,205,2049,1714,2049,1590,2049,199,16,10,1841,1874,181,118,97,114,0,2049,1853,1,0,2049,134,10,1867,1891,181,118,97,114,60,110,62,0,2049,1853,2049,134,10,1881,1905,181,99,111,110,115,116,0,2049,1853,2049,1590,2049,199,16,10,1896,1921,181,84,82,85,69,0,1,-1,10,1913,1933,181,70,65,76,83,69,0,1,0,10,1924,1947,181,110,58,122,101,114,111,63,0,1,0,11,10,1936,1963,181,110,58,45,122,101,114,111,63,0,1,0,12,10,1951,1982,181,110,58,110,101,103,97,116,105,118,101,63,0,1,0,13,10,1967,2001,181,110,58,112,111,115,105,116,105,118,101,63,0,1,0,14,10,1986,2012,181,100,105,112,0,4,5,8,6,10,2005,2024,181,115,105,112,0,5,2,6,4,1,21,2049,2012,10,2017,2039,181,98,105,0,1,2024,2049,2012,8,10,2033,2052,181,98,105,42,0,1,2012,2049,2012,8,10,2045,2065,181,98,105,64,0,2,2049,2052,10,2058,2076,181,116,114,105,0,1,2086,7,1,2024,2049,2012,2049,2024,10,1,2079,2049,2012,8,10,2069,2100,181,116,114,105,42,0,1,2119,7,1,2112,7,4,1,2012,2049,2012,10,1,2106,2049,2012,2049,2012,10,1,2103,2049,2012,8,10,2092,2133,181,116,114,105,64,0,2,2,2049,2100,10,2125,2147,181,119,104,105,108,101,0,1,2160,7,2,2049,2012,4,25,3,1,2150,7,10,1,2150,8,3,10,2138,2174,181,117,110,116,105,108,0,1,2190,7,2,2049,2012,4,1,-1,23,25,3,1,2177,7,10,1,2177,8,3,10,2165,2204,181,116,105,109,101,115,0,4,1,2222,7,25,1,1,18,5,1,21,2049,2024,6,1,2208,7,10,1,2208,8,3,10,2195,2241,181,99,111,109,112,105,108,105,110,103,63,0,1,155,15,10,2227,2254,181,100,101,112,116,104,0,1,-1,15,10,2245,2267,181,114,101,115,101,116,0,2049,2254,1,2274,7,3,10,1,2272,2049,2204,10,2258,2287,181,116,117,99,107,0,2,5,4,6,10,2279,2300,181,111,118,101,114,0,5,2,6,4,10,2292,2317,181,100,117,112,45,112,97,105,114,0,2049,2300,2049,2300,10,2305,2329,181,110,105,112,0,4,3,10,2322,2345,181,100,114,111,112,45,112,97,105,114,0,3,3,10,2332,2356,181,63,100,117,112,0,2,25,10,2348,2366,181,114,111,116,0,1,2371,7,4,10,1,2369,2049,2012,4,10,2359,2385,181,116,111,114,115,0,6,6,2,5,4,5,10,2377,2397,181,47,0,20,4,3,10,2392,2408,181,109,111,100,0,20,3,10,2401,2417,181,42,47,0,5,19,6,2049,2397,10,2411,2430,181,110,111,116,0,1,-1,23,10,2423,2443,181,110,58,112,111,119,0,1,1,4,1,2453,7,2049,2300,19,10,1,2449,2049,2204,2049,2329,10,2434,2472,181,110,58,110,101,103,97,116,101,0,1,-1,19,10,2460,2488,181,110,58,115,113,117,97,114,101,0,2,19,10,2476,2501,181,110,58,115,113,114,116,0,1,1,1,2523,7,2049,2317,2049,2397,2049,2300,18,1,2,2049,2397,25,17,1,2506,7,10,1,2506,8,2049,2329,10,2491,2538,181,110,58,109,105,110,0,2049,2317,13,1,2546,7,3,10,1,2544,1,2554,7,2049,2329,10,1,2551,2049,122,10,2529,2568,181,110,58,109,97,120,0,2049,2317,14,1,2576,7,3,10,1,2574,1,2584,7,2049,2329,10,1,2581,2049,122,10,2559,2598,181,110,58,97,98,115,0,2,2049,2472,2049,2568,10,2589,2615,181,110,58,108,105,109,105,116,0,4,5,2049,2538,6,2049,2568,10,2604,2632,181,110,58,105,110,99,0,1,1,17,10,2623,2645,181,110,58,100,101,99,0,1,1,18,10,2636,2663,181,110,58,98,101,116,119,101,101,110,63,0,2049,2366,1,2675,7,2049,2366,2049,2366,2049,2615,10,1,2668,2049,2024,11,10,2649,2693,181,118,58,105,110,99,45,98,121,0,1,2699,7,15,17,10,1,2696,2049,2024,16,10,2681,2717,181,118,58,100,101,99,45,98,121,0,1,2724,7,15,4,18,10,1,2720,2049,2024,16,10,2705,2739,181,118,58,105,110,99,0,1,1,4,2049,2693,10,2730,2754,181,118,58,100,101,99,0,1,1,4,2049,2717,10,2745,2771,181,118,58,108,105,109,105,116,0,5,5,2,15,6,6,2049,2615,4,16,10,2760,2791,181,97,108,108,111,116,0,1,3,2049,2693,10,2782,2814,181,118,58,117,112,100,97,116,101,45,117,115,105,110,103,0,4,1,2822,7,15,4,8,10,1,2818,2049,2024,16,10,2796,2841,181,83,99,111,112,101,76,105,115,116,0,4496,4578,10,2828,2850,181,123,123,0,2049,1590,2,1,2841,2049,71,16,10,2844,2875,181,45,45,45,114,101,118,101,97,108,45,45,45,0,2049,1590,1,2841,2049,2632,16,10,2859,2889,181,125,125,0,1,2841,2049,68,4,15,11,1,2906,7,1,2841,15,1,2,16,10,1,2899,1,2939,7,1,2841,15,1,2934,7,1,2,15,2,15,1,2841,2049,2632,15,12,25,3,1,2919,7,10,1,2917,8,16,10,1,2911,2049,122,10,2883,2954,164,66,117,102,102,101,114,0,0,10,2944,2963,164,80,116,114,0,0,10,2956,2978,181,116,101,114,109,105,110,97,116,101,0,1,0,1,2963,15,16,10,2883,3001,181,98,117,102,102,101,114,58,115,116,97,114,116,0,1,2954,15,10,2985,3019,181,98,117,102,102,101,114,58,101,110,100,0,1,2963,15,10,3005,3037,181,98,117,102,102,101,114,58,97,100,100,0,2049,3019,16,1,2963,2049,2739,2049,2978,10,3023,3061,181,98,117,102,102,101,114,58,103,101,116,0,1,2963,2049,2754,2049,3019,15,2049,2978,10,3047,3087,181,98,117,102,102,101,114,58,101,109,112,116,121,0,2049,3001,1,2963,16,2049,2978,10,3071,3110,181,98,117,102,102,101,114,58,115,105,122,101,0,2049,3019,2049,3001,18,10,3095,3130,181,98,117,102,102,101,114,58,115,101,116,0,1,2954,16,2049,3087,10,3116,3145,181,108,97,116,101,114,0,6,6,4,5,5,10,3136,3159,181,99,111,112,121,0,1,3169,7,1,68,2049,2012,2049,71,10,1,3162,2049,2204,3,3,10,3151,3190,181,77,65,88,45,76,69,78,71,84,72,0,1,128,10,3176,3208,164,115,116,114,58,67,117,114,114,101,110,116,0,1,10,3193,3225,181,115,116,114,58,112,111,105,110,116,101,114,0,1,3208,15,2049,3190,19,2049,1557,17,10,3210,3247,181,115,116,114,58,110,101,120,116,0,1,3208,2049,2739,1,3208,15,1,12,11,1,3266,7,1,0,1,3208,16,10,1,3260,2049,128,10,3151,3283,181,115,116,114,58,116,101,109,112,0,2,2049,79,2049,2632,2049,3225,4,2049,3159,2049,3225,2049,3247,10,3271,3311,181,115,116,114,58,101,109,112,116,121,0,2049,3225,2049,3247,10,3298,3328,181,115,116,114,58,115,107,105,112,0,6,1,3338,7,2049,68,1,0,12,10,1,3332,2049,2147,2049,2645,5,10,3316,3358,181,115,116,114,58,107,101,101,112,0,2049,2241,1,3368,7,1,3328,2049,181,10,1,3363,2049,128,2049,1714,1,3380,7,2049,149,10,1,3377,2049,2012,2049,164,10,3346,3399,195,112,114,101,102,105,120,58,39,0,2049,2241,1,3407,7,2049,3358,10,1,3404,1,3415,7,2049,3283,10,1,3412,2049,122,10,3387,3432,181,115,116,114,58,99,104,111,112,0,2049,3283,2,2049,79,2049,2300,17,2049,2645,1,0,4,16,10,3420,3462,181,115,116,114,58,114,101,118,101,114,115,101,0,2,2049,3283,2049,3130,1,79,1,3479,7,2,2049,79,17,2049,2645,10,1,3472,2049,2039,4,1,3494,7,2,15,2049,3037,2049,2645,10,1,3487,2049,2204,3,2049,3001,2049,3283,10,3447,3521,181,115,116,114,58,116,114,105,109,45,108,101,102,116,0,2049,3283,1,3550,7,2049,68,1,3535,7,1,32,11,10,1,3531,1,3544,7,1,0,12,10,1,3540,2049,2039,21,10,1,3526,2049,2147,2049,2645,10,3504,3575,181,115,116,114,58,116,114,105,109,45,114,105,103,104,116,0,2049,3283,2049,3462,2049,3521,2049,3462,10,3557,3596,181,115,116,114,58,116,114,105,109,0,2049,3575,2049,3521,10,3584,3616,181,115,116,114,58,112,114,101,112,101,110,100,0,2049,3283,1,3642,7,2,2049,79,17,1,3634,7,2,2049,79,2049,2632,10,1,3628,2049,2012,4,2049,3159,10,1,3621,2049,2024,10,3601,3661,181,115,116,114,58,97,112,112,101,110,100,0,4,2049,3616,10,3647,3675,164,78,101,101,100,108,101,0,0,10,3647,3694,181,115,116,114,58,104,97,115,45,99,104,97,114,63,0,1,3675,16,2049,68,2,1,0,11,1,3713,7,3,3,1,0,1,0,10,1,3706,1,3721,7,1,-1,10,1,3718,2049,122,25,3,1,3675,15,11,1,3739,7,1,-1,1,0,10,1,3734,1,3747,7,1,-1,10,1,3744,2049,122,25,3,1,3697,7,10,3677,3771,181,60,115,116,114,58,104,97,115,104,62,0,5,1,33,19,6,2049,68,25,4,5,17,6,1,3771,7,10,3677,3799,181,115,116,114,58,104,97,115,104,0,1,5381,4,2049,3771,3,10,3787,3819,181,99,104,114,58,83,80,65,67,69,0,1,32,10,3806,3833,181,99,104,114,58,69,83,67,0,1,27,10,3822,3847,181,99,104,114,58,84,65,66,0,1,9,10,3836,3860,181,99,104,114,58,67,82,0,1,13,10,3850,3873,181,99,104,114,58,76,70,0,1,10,10,3863,3891,181,99,104,114,58,108,101,116,116,101,114,63,0,1,65,1,122,2049,2663,10,3876,3916,181,99,104,114,58,108,111,119,101,114,99,97,115,101,63,0,1,97,1,122,2049,2663,10,3898,3941,181,99,104,114,58,117,112,112,101,114,99,97,115,101,63,0,1,65,1,90,2049,2663,10,3923,3962,181,99,104,114,58,100,105,103,105,116,63,0,1,48,1,57,2049,2663,10,3948,3988,181,99,104,114,58,119,104,105,116,101,115,112,97,99,101,63,0,1,3995,7,2049,3819,11,10,1,3991,1,4004,7,1,9,11,10,1,4000,1,4031,7,1,4016,7,1,10,11,10,1,4012,1,4025,7,1,13,11,10,1,4021,2049,2039,22,10,1,4009,2049,2076,22,22,10,3969,4054,181,99,104,114,58,116,111,45,117,112,112,101,114,0,2049,3819,18,10,4038,4074,181,99,104,114,58,116,111,45,108,111,119,101,114,0,2049,3819,17,10,4058,4097,181,99,104,114,58,116,111,103,103,108,101,45,99,97,115,101,0,2,2049,3916,1,4106,7,2049,4054,10,1,4103,1,4114,7,2049,4074,10,1,4111,2049,122,10,4078,4136,181,99,104,114,58,116,111,45,115,116,114,105,110,103,0,2049,3328,46,0,1,4138,2049,3283,1,4149,7,16,10,1,4147,2049,2024,10,4119,4170,181,99,104,114,58,118,105,115,105,98,108,101,63,0,1,31,1,126,2049,2663,10,4154,4186,181,86,97,108,117,101,0,0,10,4154,4203,181,110,58,116,111,45,115,116,114,105,110,103,0,2049,1714,2049,3130,2,1,4186,16,2049,2598,1,4229,7,1,10,20,4,1,48,17,2049,3037,2,2049,1963,10,1,4216,2049,2147,3,1,4186,15,2049,1982,1,4247,7,1,45,2049,3037,10,1,4242,2049,128,2049,3001,2049,3462,2049,3283,10,4188,4266,181,99,111,110,115,0,2049,1714,1,4277,7,4,2049,134,2049,134,10,1,4271,2049,2012,10,4258,4291,181,99,117,114,114,121,0,2049,1714,1,4304,7,4,2049,1733,2049,1779,2049,1801,10,1,4296,2049,2012,10,4282,4317,181,99,97,115,101,0,1,4324,7,2049,2300,11,10,1,4320,2049,2012,4,1,4338,7,2049,2329,8,1,-1,10,1,4332,1,4347,7,3,1,0,10,1,4343,2049,122,25,6,3,3,10,4309,4372,181,115,116,114,58,102,111,114,45,101,97,99,104,0,1,4422,7,2049,2300,15,25,3,2049,2317,1,4404,7,1,4399,7,1,4393,7,15,10,1,4391,2049,2012,8,10,1,4388,2049,2012,10,1,4385,2049,2012,1,4414,7,2049,2632,10,1,4411,2049,2012,1,4375,7,10,1,4375,8,2049,2345,10,4356,4436,181,100,111,101,115,0,2049,1608,4,2049,4291,2049,1590,2049,199,16,1,181,2049,1669,10,4428,4466,181,83,121,115,116,101,109,83,116,97,116,101,0,0,0,0,10,4428,4478,181,109,97,114,107,0,1,3,15,1,4466,1,0,17,16,2049,1590,1,4466,1,1,17,16,10,4470,4505,181,115,119,101,101,112,0,1,4466,1,0,17,15,1,3,16,1,4466,1,1,17,15,1,2,16,10,4496,4534,164,86,97,108,117,101,115,0,0,0,0,0,0,0,0,0,0,4524,4551,181,102,114,111,109,0,2049,79,2,1,4572,7,1,4565,7,1,4534,17,16,10,1,4560,2049,2024,2049,2645,10,1,4557,2049,2204,3,10,4543,4584,181,116,111,0,2,2049,79,1,4603,7,2049,68,1,97,18,2049,2632,1,4534,17,15,4,10,1,4590,2049,2204,3,10,4496,4620,181,114,101,111,114,100,101,114,0,1,4626,7,2049,4551,10,1,4623,2049,2012,2049,4584,10,4609,4641,181,112,117,116,99,0,1000,10,4633,4649,181,110,108,0,2049,3860,2049,4641,10,4643,4662,181,112,117,116,115,0,1,4668,7,2049,4641,10,1,4665,2049,4372,10,4654,4681,181,112,117,116,110,0,2049,4203,2049,4662,2049,3819,2049,4641,10,0 };

@end

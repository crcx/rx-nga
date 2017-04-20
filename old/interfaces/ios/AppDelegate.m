#import "AppDelegate.h"
#import "ViewController.h"

@interface AppDelegate ()
@end

@implementation AppDelegate

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    self.window = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
    self.rootController = [[ViewController alloc] init];
    
    [self.window addSubview:self.rootController.view];
    [self.window setRootViewController:self.rootController];
    
    [self.window makeKeyAndVisible];
    
    return YES;
}

- (void)applicationDidEnterBackground:(UIApplication *)application {
    [[NSUserDefaults standardUserDefaults] synchronize];
}

// retro://?source=...
- (BOOL)application:(UIApplication *)application openURL:(NSURL *)url
  sourceApplication:(NSString *)sourceApplication annotation:(id)annotation
{
    NSString *scheme = [[url scheme] stringByRemovingPercentEncoding];
    NSString *query = [[url query] stringByRemovingPercentEncoding];
    if ([query length] > 7) {
        NSString *source = [[NSUserDefaults standardUserDefaults] stringForKey:@"Input"];
        NSString *add = [[NSString stringWithString:[query substringFromIndex:7]] stringByRemovingPercentEncoding];
        [[NSUserDefaults standardUserDefaults] setObject:[NSString stringWithFormat:@"%@\n\n----\n\n%@",
                                                          source, add]
                                                  forKey:@"Input"];
        NSLog(@"%@", add);
    }
    NSLog(@"Calling Application Bundle ID: %@", sourceApplication);
    NSLog(@"URL scheme:%@", scheme);
    NSLog(@"URL query: %@, %@", query, [query substringFromIndex:7]);

    ViewController* mainController = (ViewController*)  self.window.rootViewController;
    [mainController loadCachedInput];
    return YES;
}
@end

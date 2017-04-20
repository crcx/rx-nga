//
//  SolarizedColors.m
//  RETRO12
//
//  Created by Charles Childers on 11/1/16.
//  Copyright © 2016 Charles Childers. All rights reserved.
//

#import "SolarizedColors.h"

@implementation SolarizedColors

#define RGB(R, G, B) [UIColor colorWithRed:R/255.0f green:G/255.0f blue:B/255.0f alpha:1.0f]

- (UIColor *)base03 {
    return RGB(0,43,54);
}

- (UIColor *)base02 {
    return RGB(7,54,66);
}

- (UIColor *)base01 {
    return RGB(88,110,117);
}

- (UIColor *)base0 {
    return RGB(131,148,150);
}

- (UIColor *)base1 {
    return RGB(147,161,161);
}

- (UIColor *)base2 {
    return RGB(238,232,213);
}

- (UIColor *)base3 {
    return RGB(253,246,227);
}

- (UIColor *)orange {
    return RGB(203,75,22);
}

#pragma mark - theme support

- (UIColor *)inputBackground {
    if (![[NSUserDefaults standardUserDefaults] boolForKey:@"wantsDark"])
        return [self base03];
    else
        return [self base3];
}

- (UIColor *)inputText {
    if (![[NSUserDefaults standardUserDefaults] boolForKey:@"wantsDark"])
        return [self base0];
    else
        return [self base02];
}


- (UIColor *)outputBackground {
    if (![[NSUserDefaults standardUserDefaults] boolForKey:@"wantsDark"])
        return [self base02];
    else
        return [self base2];
}


- (UIColor *)outputText {
    if (![[NSUserDefaults standardUserDefaults] boolForKey:@"wantsDark"])
        return [self base0];
    else
        return [self base02];
}


- (UIColor *)buttonText {
    if (![[NSUserDefaults standardUserDefaults] boolForKey:@"wantsDark"])
        return [self base03];
    else
        return [self base03];
}

@end

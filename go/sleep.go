// -*- indent-tabs-mode: nil; tab-width: 4; -*-

package ppi

import (
    "context"
    "os"
    "os/signal"
    "time"
)

func Go_SleepPi(parent context.Context, duration time.Duration) error {
    ctx, stop := signal.NotifyContext(parent, os.Interrupt)
    defer stop()

    select {
        case <-time.After(duration):
            return nil
        case <-ctx.Done():
            return ctx.Err()
    }
}

module LordOfTheRings.Ally.Cards.RavenhillScout (ravenhillScout) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype RavenhillScout = RavenhillScout AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ravenhillScout :: AllyCard RavenhillScout
ravenhillScout = ally RavenhillScout Cards.ravenhillScout

-- | Action: Exhaust Ravenhill Scout to move up to 2 progress tokens from 1 location to
-- another location.
instance HasAbilities RavenhillScout where
  getAbilities (RavenhillScout a) = [mkAbility a 1 $ ActionAbility (exhaustCost a)]

instance RunMessage RavenhillScout where
  runMessage msg s@(RavenhillScout a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      locations <- traverse getLocation =<< select AnyLocation
      chooseOneM pid do
        questionLabeled "ravenhillScout.from"
        causedBy a
        targets [l.id | l <- locations, l.progress > 0] \from -> do
          progress <- (.progress) <$> getLocation from
          chooseOneM pid do
            questionLabeled "ravenhillScout.to"
            causedBy a
            targets [l.id | l <- locations, l.id /= from] \dest ->
              chooseAmountM pid "ravenhillScout.amount" a 1 (min 2 progress) \n -> do
                removeTokens a from Progress n
                placeProgress a dest n
      pure s
    _ -> RavenhillScout <$> runMessage msg a

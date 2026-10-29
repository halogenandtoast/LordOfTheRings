module LordOfTheRings.Ally.Cards.Saruman (saruman) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (cdUnique, toCardDef)

newtype Saruman = Saruman AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 3, on its card def.
saruman :: AllyCard Saruman
saruman = ally Saruman Cards.saruman

-- | At the end of the round, discard Saruman from play.
instance HasModifiersFor Saruman where
  getModifiersFor (Saruman a) = modifySelf a [DiscardAtEndOfRound]

-- | Response: After Saruman enters play, choose a non-unique enemy or location in the
-- staging area. While Saruman is in play, the chosen enemy or location is considered to be
-- out of play.
instance HasAbilities Saruman where
  getAbilities (Saruman a) = [mkAbility a 1 $ response $ entersPlay a]

instance RunMessage Saruman where
  runMessage msg s@(Saruman a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (EnemyInStagingArea <> not_ UniqueEnemy <> EnemyNotImmuneToPlayerCards)
      locations <-
        filterM (fmap (not . cdUnique . toCardDef) . getLocation)
          =<< select (LocationInStagingArea <> LocationNotImmuneToPlayerCards)
      -- the card's text is blank while it is out of play; the effect ends as Saruman leaves
      -- ponytail: no matcher finds it, but the board still shows it in the staging area, and
      -- what walks every enemy or location without a matcher (regenerate at the start of the
      -- refresh phase, time counters at its end) still touches it
      chooseOneM pid do
        questionLabeled "saruman.choose"
        causedBy a
        targets enemies \e -> roundModifiers a e [ConsideredOutOfPlay, Blank]
        targets locations \l -> roundModifiers a l [ConsideredOutOfPlay, Blank]
      pure s
    _ -> Saruman <$> runMessage msg a

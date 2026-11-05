module LordOfTheRings.Ally.Cards.EagleOfTheNorth (eagleOfTheNorth) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype EagleOfTheNorth = EagleOfTheNorth AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter. Surge, on its card def: revealed from the encounter deck, it enters play
-- under the first player (see the game's reveal).
eagleOfTheNorth :: AllyCard EagleOfTheNorth
eagleOfTheNorth = ally EagleOfTheNorth Cards.eagleOfTheNorth

-- | Cannot have restricted attachments. A player card with the encounter keyword that
-- leaves play is removed from the game (MEC67 rules).
instance HasModifiersFor EagleOfTheNorth where
  getModifiersFor (EagleOfTheNorth a) = modifySelf a [CannotHaveRestrictedAttachments, RemoveFromGameIfLeavesPlay]

-- | When Revealed: The first player chooses a player to take control of Eagle of the
-- North. The chosen player may choose and discard a non-unique enemy in the staging area.
instance RunMessage EagleOfTheNorth where
  runMessage msg s@(EagleOfTheNorth a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      lead <- getLead
      choosePlayerM lead a \pid -> do
        when (pid /= lead) $ takeControl a.id pid
        enemies <- select (EnemyInStagingArea <> NotEnemy UniqueEnemy <> EnemyNotImmuneToPlayerCards)
        chooseOneM pid do
          questionLabeled "eagleOfTheNorth.discard"
          causedBy a
          targets enemies (discard a)
          labeled "eagleOfTheNorth.none" $ pure ()
      pure s
    _ -> EagleOfTheNorth <$> runMessage msg a

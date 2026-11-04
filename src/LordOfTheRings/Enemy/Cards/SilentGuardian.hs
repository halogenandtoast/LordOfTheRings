module LordOfTheRings.Enemy.Cards.SilentGuardian (silentGuardian) where

import LordOfTheRings.Card (Card (PlayerCard), CardType (EventType), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype SilentGuardian = SilentGuardian EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

silentGuardian :: EnemyCard SilentGuardian
silentGuardian = enemy SilentGuardian Cards.silentGuardian

-- | While you are engaged with Silent Guardian it gains: "Forced: The first time you play an
-- event card each round, instead of discarding that card after playing it, reanimate it."
-- Read as the event lies in its player's discard pile, once it is played; a round effect on
-- the player marks it used.
instance RunMessage SilentGuardian where
  runMessage msg (SilentGuardian attrs) = case msg of
    CheckWindows ws | EngagedWith pid <- attrs.placement -> do
      player <- getPlayer pid
      used <- elem marker <$> getModifiers (PlayerTarget pid)
      let played = [cid | Window After (PlayedFromHand (CardIdTarget cid)) <- ws]
          events = [c.id | c@(PlayerCard pc) <- player.discard, pc.id `elem` played, c.cardType == EventType]
      unless used $ for_ (take 1 events) \cid -> do
        roundModifier attrs (PlayerTarget pid) marker
        void $ reanimate cid
      SilentGuardian <$> runMessage msg attrs
    _ -> SilentGuardian <$> runMessage msg attrs
   where
    marker = PlayedThisRound (toCardCode attrs)

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
-- Read as the event lies in its player's discard pile, once it is played: the first of the
-- round when his events this round number 1, those played before he engaged it counted.
instance RunMessage SilentGuardian where
  runMessage msg (SilentGuardian attrs) = case msg of
    CheckWindows ws | EngagedWith pid <- attrs.placement -> do
      player <- getPlayer pid
      played <- length . filter (== PlayedEventThisRound) <$> getModifiers (PlayerTarget pid)
      let fromHand = [cid | Window After (PlayedFromHand (CardIdTarget cid)) <- ws]
          -- one played from elsewhere is known by its code, the last of it discarded
          elsewhere = [code | null fromHand, Window After (PlayedCard p code) <- ws, p == pid]
          events =
            [c.id | c@(PlayerCard pc) <- player.discard, pc.id `elem` fromHand, c.cardType == EventType]
              <> take 1 [c.id | c <- player.discard, toCardCode c `elem` elsewhere, c.cardType == EventType]
      when (played == 1) $ for_ (take 1 events) (void . reanimate)
      SilentGuardian <$> runMessage msg attrs
    _ -> SilentGuardian <$> runMessage msg attrs

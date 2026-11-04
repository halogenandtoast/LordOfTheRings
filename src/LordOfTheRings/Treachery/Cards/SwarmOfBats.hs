module LordOfTheRings.Treachery.Cards.SwarmOfBats (swarmOfBats) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SwarmOfBats = SwarmOfBats TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

swarmOfBats :: TreacheryCard SwarmOfBats
swarmOfBats = treachery SwarmOfBats Cards.swarmOfBats

-- | When Revealed: Each player discards each resource from each of his heroes' resource
-- pools and raises his threat by 1 for each resource he discarded this way. If no resources
-- were discarded this way, Swarm of Bats gains surge.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if the defending character has
-- an attachment).
instance RunMessage SwarmOfBats where
  runMessage msg t@(SwarmOfBats attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      discarded <- for players \p -> do
        heroes <- select (HeroControlledBy $ PlayerWithId p)
        pools <- for heroes \h -> (h,) . (.resources) <$> getHero h
        for_ [(h, n) | (h, n) <- pools, n > 0] \(h, n) -> removeTokens attrs h Resource n
        let n = sum (map snd pools)
        when (n > 0) $ encounterRaisesThreat p n
        pure n
      when (sum discarded == 0) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      equipped <- anyM (fmap notNull . getAllAttachmentsOn) defenders
      enemyGetsAttack attrs eid (if equipped then 2 else 1)
      pure t
    _ -> SwarmOfBats <$> runMessage msg attrs

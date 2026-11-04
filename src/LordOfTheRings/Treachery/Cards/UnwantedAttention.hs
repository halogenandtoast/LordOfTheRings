module LordOfTheRings.Treachery.Cards.UnwantedAttention (unwantedAttention) where

import LordOfTheRings.Scenario.Scenarios.AKnifeInTheDark.Helpers (eitherRingwraithOr)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UnwantedAttention = UnwantedAttention TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril.
unwantedAttention :: TreacheryCard UnwantedAttention
unwantedAttention = treachery UnwantedAttention Cards.unwantedAttention

{- | When Revealed: You must either remove 2 heroes you control from the quest or shuffle an
out of play Ringwraith into the encounter deck. "You" is the player who revealed it, who
chooses on his own (Peril); the heroes are removable in full only with 2 committed.
Shadow: Return attacking enemy to the staging area after this attack.
-}
instance RunMessage UnwantedAttention where
  runMessage msg t@(UnwantedAttention attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      heroes <- select (CommittedHero <> HeroControlledBy (PlayerWithId you))
      eitherRingwraithOr you attrs "unwantedAttention.choose" (length heroes >= 2) "unwantedAttention.remove"
        $ removeHeroes you 2 heroes
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure t
    _ -> UnwantedAttention <$> runMessage msg attrs
   where
    removeHeroes :: ReverseQueue m => PlayerId -> Int -> [HeroId] -> m ()
    removeHeroes you n heroes
      | length heroes <= n = traverse_ removeFromQuest heroes
      | otherwise = chooseOneM you do
          questionLabeled "unwantedAttention.removeHero"
          causedBy attrs
          targets heroes \h -> do
            removeFromQuest h
            removeHeroes you (n - 1) (filter (/= h) heroes)

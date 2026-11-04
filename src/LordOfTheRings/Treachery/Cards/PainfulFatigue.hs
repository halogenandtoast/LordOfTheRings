module LordOfTheRings.Treachery.Cards.PainfulFatigue (painfulFatigue) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PainfulFatigue = PainfulFatigue TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

painfulFatigue :: TreacheryCard PainfulFatigue
painfulFatigue = treachery PainfulFatigue Cards.painfulFatigue

-- | When Revealed: Each player exhausts a character he controls and deals 1 damage to it: a
-- ready one, as only it can be exhausted.
-- Shadow: Either exhaust a character you control, or return attacking enemy to the staging
-- area after this attack.
instance RunMessage PainfulFatigue where
  runMessage msg t@(PainfulFatigue attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        able <- readyCharactersOf p
        chooseOrRunOneM p do
          questionLabeled "painfulFatigue.exhaust"
          causedBy attrs
          targets able \c -> do
            exhaust c
            dealDamage attrs c 1
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      able <- readyCharactersOf pid
      chooseOneM pid do
        questionLabeled "painfulFatigue.choose"
        causedBy attrs
        when (notNull able) $ labeled "painfulFatigue.exhaustCharacter" $ chooseAndExhaust attrs pid 1
        labeled "painfulFatigue.return" $ doStep 1 msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure t
    _ -> PainfulFatigue <$> runMessage msg attrs
   where
    readyCharactersOf p = selectCharacters (HeroControlledBy (PlayerWithId p) <> ReadyHero) (AllyControlledBy (PlayerWithId p) <> ReadyAlly)

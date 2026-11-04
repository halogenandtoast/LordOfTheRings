module LordOfTheRings.Treachery.Cards.BlackDart (blackDart) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields.Helpers (attachedCopies)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BlackDart = BlackDart TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril.
blackDart :: TreacheryCard BlackDart
blackDart = treachery BlackDart Cards.blackDart

-- | Counts as a Condition attachment with the text: "Forced: At the end of the round, deal 1
-- damage to attached hero."
instance HasAbilities BlackDart where
  getAbilities (BlackDart a) = [mkAbility a 1 $ forced RoundEndsWindow | AttachedTo (HeroTarget _) <- [a.placement]]

-- | The heroes a copy can go on: Limit 1 per hero.
withoutDart :: HasGame m => [HeroId] -> m [HeroId]
withoutDart = filterM (fmap (== 0) . attachedCopies Cards.blackDart)

{- | When Revealed: Remove a hero you control from the quest and attach Black Dart to that
hero. Limit 1 per hero.
Shadow: If this attack damages a hero, attach Black Dart to that hero. Read with the
attack's damage, while the shadow card is still dealt; the defending player chooses among
several.
-}
instance RunMessage BlackDart where
  runMessage msg t@(BlackDart attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      heroes <- withoutDart =<< select (HeroControlledBy $ PlayerWithId you)
      chooseOrRunOneM you do
        questionLabeled "blackDart.attach"
        causedBy attrs
        targets heroes \h -> do
          removeFromQuest h
          attachTreachery attrs.treacheryId h
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ [h | AttachedTo h@(HeroTarget _) <- [attrs.placement]] \h -> dealDamage attrs h 1
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      withAttackDamage eid [DoStep 1 msg]
      pure t
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == attrs.cardId -> do
      damaged <- attackDamaged eid
      inPlay <- select AnyHero
      heroes <- withoutDart (ordNub [h | (HeroTarget h, n) <- damaged, n > 0, h `elem` inPlay])
      chooseOrRunOneM pid do
        questionLabeled "blackDart.attach"
        causedBy attrs
        targets heroes $ attachShadowTreachery eid cid
      pure t
    _ -> BlackDart <$> runMessage msg attrs

module LordOfTheRings.Attachment.Cards.GandalfsStaff (gandalfsStaff) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype GandalfsStaff = GandalfsStaff AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to Gandalf. Restricted, on its card def.
gandalfsStaff :: AttachmentCard GandalfsStaff
gandalfsStaff = attachment GandalfsStaff Cards.gandalfsStaff & attachTo (AttachToCharacterMatching (HeroWithTitle "Gandalf") (AllyWithTitle "Gandalf"))

-- | Action: Exhaust Gandalf's Staff to (choose one): choose a player to draw 1 card, add 1
-- resource to a hero's resource pool, or discard a shadow card from a non-unique enemy.
instance HasAbilities GandalfsStaff where
  getAbilities (GandalfsStaff a) = [mkAbility a 1 $ ActionAbility (exhaustCost a)]

instance RunMessage GandalfsStaff where
  runMessage msg s@(GandalfsStaff a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- select AnyHero
      enemies <- traverse getEnemy =<< select (NotEnemy UniqueEnemy <> EnemyNotImmuneToPlayerCards)
      shadowed <- filter (notNull . snd) <$> for enemies \e -> (e,) <$> shadowsPlayerCardsCanAffect e
      chooseOneM pid do
        questionLabeled "gandalfsStaff.chooseOne"
        causedBy a
        labeled "gandalfsStaff.draw" $ choosePlayerM pid a \p -> drawCards a p 1
        labeled "gandalfsStaff.resource" $ chooseOrRunOneM pid do
          questionLabeled "chooseHero"
          causedBy a
          targets heroes \h -> placeResources a h 1
        when (notNull shadowed) $ labeled "gandalfsStaff.shadow" $ chooseOneM pid do
          questionLabeled "gandalfsStaff.discardShadow"
          causedBy a
          for_ shadowed \(e, shadows) -> targets shadows \c -> discardShadowCard e.id c
      pure s
    _ -> GandalfsStaff <$> runMessage msg a

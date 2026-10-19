module LordOfTheRings.Hero.Cards.Glorfindel (glorfindel) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Glorfindel = Glorfindel HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

glorfindel :: HeroCard Glorfindel
glorfindel = hero Glorfindel Cards.glorfindel

-- | Action: Pay 1 resource from Glorfindel's pool to heal 1 damage on any character.
-- (Limit once per round.)
instance HasAbilities Glorfindel where
  getAbilities (Glorfindel a) =
    [limited (GroupLimit PerRound 1) $ mkAbility a 1 $ ActionAbility (ResourceCost (toTarget a) 1)]

instance RunMessage Glorfindel where
  runMessage msg h@(Glorfindel attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      damaged <- damagedCharacters AnyHero AnyAlly
      unless (null damaged)
        $ push
        $ questionLabelWith "Heal 1 damage on a character" (Just $ toTarget attrs) [] pid
          [targetLabel c [HealDamage (toSource attrs) c 1] | c <- damaged]
      pure h
    _ -> Glorfindel <$> runMessage msg attrs

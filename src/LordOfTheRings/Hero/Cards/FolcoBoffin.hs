module LordOfTheRings.Hero.Cards.FolcoBoffin (folcoBoffin) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit))

newtype FolcoBoffin = FolcoBoffin HeroAttrs
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, Entity)

folcoBoffin :: HeroCard FolcoBoffin
folcoBoffin = hero FolcoBoffin Cards.folcoBoffin

-- | Folco Boffin gets -1 threat cost for each Hobbit hero you control (himself among them).
instance HasModifiersFor FolcoBoffin where
  getModifiersFor (FolcoBoffin a) = do
    hobbits <- length <$> select (HeroControlledBy (PlayerWithId a.owner) <> HeroWithTrait Hobbit)
    when (hobbits > 0) $ modifySelf a [ThreatModifier (negate hobbits)]

-- | Action: Discard Folco Boffin to reduce your threat by 7. (Limit once per game.)
instance HasAbilities FolcoBoffin where
  getAbilities (FolcoBoffin a) = [limited (PlayerLimit PerGame 1) $ mkAbility a 1 $ ActionAbility Free]

instance RunMessage FolcoBoffin where
  runMessage msg h@(FolcoBoffin attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      discard attrs attrs
      lowerThreatBy attrs pid 7
      pure h
    _ -> FolcoBoffin <$> runMessage msg attrs

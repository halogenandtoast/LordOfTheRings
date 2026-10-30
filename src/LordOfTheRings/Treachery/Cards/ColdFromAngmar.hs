module LordOfTheRings.Treachery.Cards.ColdFromAngmar (coldFromAngmar) where

import LordOfTheRings.Card (cdKeywords, toCardDef)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ColdFromAngmar = ColdFromAngmar TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

coldFromAngmar :: TreacheryCard ColdFromAngmar
coldFromAngmar = treachery ColdFromAngmar Cards.coldFromAngmar

-- | Counts as a Condition attachment with the text: "Treat each damaged character's printed
-- text box as if it were blank, except for keywords and Traits." Its printed keywords it
-- keeps as gained ones.
instance HasModifiersFor ColdFromAngmar where
  getModifiersFor (ColdFromAngmar a) = case a.placement of
    AttachedTo _ -> do
      heroes <- traverse getHero =<< select AnyHero
      allies <- traverse getAlly =<< select AnyAlly
      let blank damage target def = when (damage > 0) $ modified a target (Blank : map GainKeyword (toList $ cdKeywords def))
      for_ heroes \h -> blank h.damage (toTarget h) (toCardDef h)
      for_ allies \al -> blank al.damage (toTarget al) (toCardDef al)
    _ -> pure ()

-- | When Revealed: Each player deals 1 damage to a hero he controls. Then, attach Cold from
-- Angmar to the current quest.
instance RunMessage ColdFromAngmar where
  runMessage msg t@(ColdFromAngmar attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> dealDamageToOneHero attrs p 1
      traverse_ (attachTreachery attrs.id) =<< getCurrentQuest
      pure t
    _ -> ColdFromAngmar <$> runMessage msg attrs

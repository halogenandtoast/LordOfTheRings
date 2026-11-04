module LordOfTheRings.Ally.Cards.PrinceImrahilSpirit (princeImrahilSpirit) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (HeroType), cdCardType, toCardDef)
import LordOfTheRings.Placement (Placement (InPlayArea))

newtype PrinceImrahilSpirit = PrinceImrahilSpirit AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

princeImrahilSpirit :: AllyCard PrinceImrahilSpirit
princeImrahilSpirit = ally PrinceImrahilSpirit Cards.princeImrahilSpirit

{- | While there is a hero in your discard pile, Prince Imrahil loses the ally card type and
gains the hero card type: checked as each message resolves, he turns to a hero as one
reaches the pile, and back to an ally once none is left there. As a hero his text is read
through 'LordOfTheRings.Hero.AllyHero', as Sword-thain's ally. Turning twice before the
first turn resolves is harmless: the game ignores a turn its card has already made.
-}
instance RunMessage PrinceImrahilSpirit where
  runMessage msg (PrinceImrahilSpirit a) = case msg of
    AllyBecomesHero _ -> PrinceImrahilSpirit <$> runMessage msg a
    HeroBecomesAlly _ -> PrinceImrahilSpirit <$> runMessage msg a
    _ | InPlayArea you <- a.placement -> do
      heroInDiscard <- any ((== HeroType) . cdCardType . toCardDef) . (.discard) <$> getPlayer you
      if a.allyAsHero
        then unless heroInDiscard $ heroBecomesAlly a
        else when heroInDiscard $ allyBecomesHero a
      PrinceImrahilSpirit <$> runMessage msg a
    _ -> PrinceImrahilSpirit <$> runMessage msg a
